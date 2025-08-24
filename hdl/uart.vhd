library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity uart is
  generic (
    BAUD_RATE   : positive; -- Desired baud rate (e.g., 115200)
    CLK_FREQ    : positive  -- System clock frequency (Hz)
  );
  port (  
    clk                 :   in  std_logic; -- System clock input
    rst                 :   in  std_logic; -- Synchronous reset (active high)
    data_stream_tx      :   in  std_logic_vector(7 downto 0); -- Byte data to transmit
    data_stream_tx_stb  :   in  std_logic; -- Transmit data valid strobe
    data_stream_tx_ack  :   out std_logic; -- Transmit data acknowledge pulse
    data_stream_rx      :   out std_logic_vector(7 downto 0); -- Received byte data output
    data_stream_rx_stb  :   out std_logic; -- Received data valid strobe
    tx                  :   out std_logic; -- UART transmit serial output
    rx                  :   in  std_logic  -- UART receive serial input
  );
end uart;

architecture rtl of uart is

  ---------------------------------------------------------------------------
  -- Baud rate generation constants
  -- C_TX_DIV: Divider for TX baud rate clock generation (clk cycles per baud)
  -- C_RX_DIV: Divider for RX oversampling clock generation (clk cycles per 16x baud)
  -- C_TX_DIV_WIDTH: Bit width needed to count up to C_TX_DIV
  -- C_RX_DIV_WIDTH: Bit width needed to count up to C_RX_DIV
  ---------------------------------------------------------------------------
  constant C_TX_DIV       : integer := CLK_FREQ / BAUD_RATE;
  constant C_RX_DIV       : integer := CLK_FREQ / (BAUD_RATE * 16);
  constant C_TX_DIV_WIDTH : integer := integer(log2(real(C_TX_DIV))) + 1;   
  constant C_RX_DIV_WIDTH : integer := integer(log2(real(C_RX_DIV))) + 1;

  -- TX baud rate counter and tick signal (one tick per baud period)
  signal tx_baud_counter    : unsigned(C_TX_DIV_WIDTH - 1 downto 0) := (others => '0');
  signal tx_baud_tick       : std_logic := '0';

  -- RX oversampling counter and tick signal (one tick per 1/16 baud period)
  signal rx_baud_counter    : unsigned(C_RX_DIV_WIDTH - 1 downto 0) := (others => '0');
  signal rx_baud_tick       : std_logic := '0';

  ---------------------------------------------------------------------------
  -- UART transmitter states:
  -- tx_send_start_bit: Sending the start bit (logic 0)
  -- tx_send_data: Sending 8 data bits (LSB first)
  -- tx_send_stop_bit: Sending stop bit (logic 1)
  ---------------------------------------------------------------------------
  type uart_tx_states is ( 
    tx_send_start_bit,
    tx_send_data,
    tx_send_stop_bit
  );             

  -- Transmitter state machine current state
  signal uart_tx_state          : uart_tx_states := tx_send_start_bit;

  -- Transmitter shift register holding byte to send
  signal uart_tx_shift_reg      : std_logic_vector(7 downto 0) := (others => '0');

  -- UART serial TX output line signal
  signal uart_tx_serial_out     : std_logic := '1'; 

  -- Bit counter for transmitted data bits (0 to 7)
  signal uart_tx_bit_count      : unsigned(2 downto 0) := (others => '0'); 

  -- Internal acknowledge signal for accepted TX data
  signal uart_tx_data_accepted  : std_logic := '0'; 

  ---------------------------------------------------------------------------
  -- UART receiver states:
  -- rx_get_start_bit: Waiting for start bit (logic 0)
  -- rx_get_data: Receiving 8 data bits
  -- rx_get_stop_bit: Checking stop bit (logic 1)
  ---------------------------------------------------------------------------
  type uart_rx_states is ( 
      rx_get_start_bit, 
      rx_get_data, 
      rx_get_stop_bit
  );            

  -- Receiver current state
  signal uart_rx_state           : uart_rx_states := rx_get_start_bit;

  -- Filtered and synchronized received RX bit
  signal uart_rx_filtered_bit    : std_logic := '1';

  -- Receiver shift register holding received byte
  signal uart_rx_shift_reg       : std_logic_vector(7 downto 0) := (others => '0'); 
  
  -- Double flop synchronizer for async RX input
  signal uart_rx_sync_reg        : std_logic_vector(1 downto 0) := (others => '1'); 
  
  -- 2-bit saturating counter for digital filtering of RX line noise
  signal uart_rx_debounce_counter: unsigned(1 downto 0) := (others => '1'); 
  
  -- Bit counter for received data bits (0 to 7)
  signal uart_rx_bit_count       : unsigned(2 downto 0) := (others => '0'); 
  
  -- Strobe pulse indicating a new valid received byte is available
  signal uart_rx_data_ready      : std_logic := '0'; 
  
  -- Counter to track bit sampling timing using oversampled clock
  signal uart_rx_sample_counter  : unsigned(3 downto 0) := (others => '0'); 
  
  -- Tick indicating when to sample the next bit
  signal uart_rx_sample_tick     : std_logic := '0'; 

begin

  data_stream_tx_ack  <= uart_tx_data_accepted;
  data_stream_rx      <= uart_rx_shift_reg;
  data_stream_rx_stb  <= uart_rx_data_ready;
  tx                  <= uart_tx_serial_out;

  ---------------------------------------------------------------------------
  -- RX_CLOCK_DIVIDER
  -- Generates a tick signal (rx_baud_tick) at 16 times the baud rate frequency.
  -- This oversampled clock is used to sample the RX input line more accurately.
  ---------------------------------------------------------------------------
  rx_clock_divider : process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        rx_baud_counter <= (others => '0');
        rx_baud_tick <= '0';
      else
        if rx_baud_counter = C_RX_DIV then
          rx_baud_counter <= (others => '0'); -- Reset counter
          rx_baud_tick <= '1'; -- Generate a baud tick pulse
        else
          rx_baud_counter <= rx_baud_counter + 1; -- Increment counter
          rx_baud_tick <= '0'; -- Clear baud tick
        end if;
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------------
  -- RX_SYNC
  -- Synchronizes the asynchronous UART RX input to the internal clock domain.
  -- Uses a two-stage shift register to reduce metastability issues.
  ---------------------------------------------------------------------------
  rx_sync : process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        uart_rx_sync_reg <= (others => '1');
      else
        if rx_baud_tick = '1' then
          uart_rx_sync_reg(0) <= rx; -- Sample RX input asynchronously
          uart_rx_sync_reg(1) <= uart_rx_sync_reg(0); -- Second stage synchronization
        end if;
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------------
  -- RX_FILTER
  -- Applies a simple digital filter to the synchronized RX input signal.
  -- Uses a 2-bit saturating counter to debounce the input and reduce noise.
  ---------------------------------------------------------------------------
  rx_filter : process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        uart_rx_debounce_counter <= (others => '1');
        uart_rx_filtered_bit <= '1';
      else
        if rx_baud_tick = '1' then
          -- Increment filter counter if sampled RX is '1' and counter not saturated
          if uart_rx_sync_reg(1) = '1' and uart_rx_debounce_counter < 3 then
            uart_rx_debounce_counter <= uart_rx_debounce_counter + 1;

          -- Decrement filter counter if sampled RX is '0' and counter above 0
          elsif uart_rx_sync_reg(1) = '0' and uart_rx_debounce_counter > 0 then
            uart_rx_debounce_counter <= uart_rx_debounce_counter - 1;
          end if;

          -- Set filtered RX bit to '1' only if counter fully saturated (3)
          if uart_rx_debounce_counter = 3 then
            uart_rx_filtered_bit <= '1';

          -- Set filtered RX bit to '0' only if counter fully zero (0)
          elsif uart_rx_debounce_counter = 0 then
            uart_rx_filtered_bit <= '0';
          end if;
        end if;
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------------
  -- RX_BIT_SAMPLING_TICK_GENERATOR
  -- Generates a tick signal (uart_rx_sample_tick) to sample UART RX bits
  -- at the correct intervals based on 16x oversampling.
  -- Resets timing counter on detecting a new start bit.
  ---------------------------------------------------------------------------
  rx_bit_sampling_tick_generator : process (clk)
  begin
    if rising_edge(clk) then
      uart_rx_sample_tick <= '0'; -- Default: no sample tick this cycle
      
      if rx_baud_tick = '1' then       
        if uart_rx_sample_counter = 15 then
          uart_rx_sample_tick <= '1';  -- Generate a sampling tick every 16 oversampled clocks
          uart_rx_sample_counter <= (others => '0'); -- Reset the sample counter
        else
          uart_rx_sample_counter <= uart_rx_sample_counter + 1; -- Increment sample counter
        end if;

        -- Reset sample counter immediately when a new start bit is detected
        if uart_rx_state = rx_get_start_bit then
          uart_rx_sample_counter <= (others => '0');
        end if; 
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------------
  -- UART_RECEIVE_DATA
  -- Implements the UART receiver state machine.
  -- Detects start bit, receives data bits, and checks stop bit.
  -- Outputs received byte with a strobe when complete.
  ---------------------------------------------------------------------------
  uart_receive_data : process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        uart_rx_state <= rx_get_start_bit;    -- Reset to wait for start bit
        uart_rx_shift_reg <= (others => '0'); -- Clear received data buffer
        uart_rx_bit_count <= (others => '0'); -- Clear bit counter
        uart_rx_data_ready <= '0';            -- Clear output strobe
      else
        uart_rx_data_ready <= '0';            -- Default no output strobe
        
        case uart_rx_state is
          when rx_get_start_bit =>
            -- Wait for start bit (logic 0)
            if rx_baud_tick = '1' and uart_rx_filtered_bit = '0' then
              uart_rx_state <= rx_get_data;   -- Start receiving data bits
            end if;

          when rx_get_data =>
            -- Sample data bits at sample tick
            if uart_rx_sample_tick = '1' then
              -- Shift new bit into MSB of shift register (LSB first)
              uart_rx_shift_reg(uart_rx_shift_reg'high) <= uart_rx_filtered_bit;
              uart_rx_shift_reg(uart_rx_shift_reg'high-1 downto 0) <= 
                uart_rx_shift_reg(uart_rx_shift_reg'high downto 1);

              if uart_rx_bit_count < 7 then
                uart_rx_bit_count <= uart_rx_bit_count + 1; -- Increment bit count
              else
                uart_rx_bit_count <= (others => '0');       -- Reset bit count
                uart_rx_state <= rx_get_stop_bit;           -- Move to stop bit check
              end if;
            end if;

          when rx_get_stop_bit =>
            -- Check stop bit (should be logic 1)
            if uart_rx_sample_tick = '1' then
              if uart_rx_filtered_bit = '1' then
                uart_rx_state <= rx_get_start_bit;  -- Ready for next start bit
                uart_rx_data_ready <= '1';          -- Output strobe: new byte ready
              end if;
            end if;

          when others =>
            uart_rx_state <= rx_get_start_bit;      -- Default to start bit detection
        end case;
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------------
  -- TX_CLOCK_DIVIDER
  -- Generates the baud rate tick for the UART transmitter.
  -- Divides the input clock frequency down to the desired baud rate.
  ---------------------------------------------------------------------------
  tx_clock_divider : process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        tx_baud_counter <= (others => '0');
        tx_baud_tick <= '0';
      else
        if tx_baud_counter = C_TX_DIV then
          tx_baud_counter <= (others => '0');     -- Reset counter when divider reached
          tx_baud_tick <= '1';                    -- Generate one-cycle baud tick
        else
          tx_baud_counter <= tx_baud_counter + 1; -- Increment counter
          tx_baud_tick <= '0';                    -- No tick this cycle
        end if;
      end if;
    end if;
  end process;

  ---------------------------------------------------------------------------
  -- UART_SEND_DATA
  -- Implements the UART transmitter state machine.
  -- Sends start bit, 8 data bits (LSB first), and stop bit at each baud tick.
  -- Acknowledges new data acceptance with uart_tx_data_accepted.
  ---------------------------------------------------------------------------
  uart_send_data : process(clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        uart_tx_serial_out <= '1';            -- Idle line is logic high
        uart_tx_shift_reg <= (others => '0'); -- Clear transmit shift register
        uart_tx_bit_count <= (others => '0'); -- Reset bit counter
        uart_tx_state <= tx_send_start_bit;   -- Wait for new data
        uart_tx_data_accepted <= '0';         -- Clear acknowledge
      else
        uart_tx_data_accepted <= '0';         -- Default no ack
        
        case uart_tx_state is
          when tx_send_start_bit =>
            -- Wait for new data to send
            if tx_baud_tick = '1' and data_stream_tx_stb = '1' then
              uart_tx_serial_out <= '0';            -- Send start bit (logic 0)
              uart_tx_state <= tx_send_data;        -- Move to sending data bits
              uart_tx_bit_count <= (others => '0'); -- Reset bit count
              uart_tx_data_accepted <= '1';         -- Acknowledge data accepted
              uart_tx_shift_reg <= data_stream_tx;  -- Load data to send
            end if;

          when tx_send_data =>
            if tx_baud_tick = '1' then
              uart_tx_serial_out <= uart_tx_shift_reg(0);  -- Send LSB first
              uart_tx_shift_reg(uart_tx_shift_reg'high-1 downto 0) <= 
                uart_tx_shift_reg(uart_tx_shift_reg'high downto 1); -- Shift right
              
              if uart_tx_bit_count < 7 then
                uart_tx_bit_count <= uart_tx_bit_count + 1; -- Increment bit count
              else
                uart_tx_bit_count <= (others => '0');       -- Reset bit count
                uart_tx_state <= tx_send_stop_bit;          -- Move to stop bit
              end if;
            end if;

          when tx_send_stop_bit =>
            if tx_baud_tick = '1' then
              uart_tx_serial_out <= '1';          -- Send stop bit (logic 1)
              uart_tx_state <= tx_send_start_bit; -- Return to idle/wait state
            end if;

          when others =>
            uart_tx_serial_out <= '1';            -- Default line idle
            uart_tx_state <= tx_send_start_bit;   -- Reset state machine
        end case;
      end if;
    end if;
  end process;

end architecture;
