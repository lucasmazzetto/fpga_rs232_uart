library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fpga_rs232_uart is
  generic (
    CLK_FREQ  : positive := 50_000_000; -- Clock frequency in Hz
    BAUD_RATE : positive := 115200      -- UART baud rate
  );
  port (
    clk       : in  std_logic; -- System clock
    rst_n     : in  std_logic; -- Active-low asynchronous reset
    uart_rxd  : in  std_logic; -- UART rx pin
    uart_txd  : out std_logic  -- UART tx pin
  );
end entity;

architecture rtl of fpga_rs232_uart is
  ---------------------------------------------------------------------------
  -- UART interface signals
  ---------------------------------------------------------------------------
  signal uart_tx_data  : std_logic_vector(7 downto 0);
  signal uart_tx_stb   : std_logic := '0';
  signal uart_tx_ack   : std_logic;

  signal uart_rx_data  : std_logic_vector(7 downto 0);
  signal uart_rx_stb   : std_logic := '0';

  ---------------------------------------------------------------------------
  -- FIFO buffer signals
  ---------------------------------------------------------------------------
  constant BUFFER_DEPTH : integer := 1024;
  signal fifo_data_in   : std_logic_vector(7 downto 0);
  signal fifo_data_out  : std_logic_vector(7 downto 0);
  signal fifo_write_en  : std_logic := '0';
  signal fifo_read_en   : std_logic := '0';
  signal fifo_full      : std_logic;
  signal fifo_empty     : std_logic;

  ---------------------------------------------------------------------------
  -- Sync and reset signals
  ---------------------------------------------------------------------------
  signal tx       : std_logic;
  signal rx       : std_logic := '1';
  signal rx_sync  : std_logic := '1';
  signal rst      : std_logic := '0';
  signal rst_sync : std_logic := '0';

  ---------------------------------------------------------------------------
  -- TX pipeline FSM (handles registered FIFO read)
  ---------------------------------------------------------------------------
  type tx_pipe_state_t is (IDLE, READ1, LATCH, SEND);
  signal txp_state : tx_pipe_state_t := IDLE;

  -- Local latch to decouple UART TX input from FIFO output bus
  signal tx_byte : std_logic_vector(7 downto 0) := (others => '0');

begin
  -----------------------------------------------------------------------------
  -- UART
  -----------------------------------------------------------------------------
  uart_inst: entity work.uart
    generic map (
      BAUD_RATE => BAUD_RATE,
      CLK_FREQ  => CLK_FREQ
    )
    port map (
      clk                 => clk,
      rst                 => rst,
      data_stream_tx      => uart_tx_data,
      data_stream_tx_stb  => uart_tx_stb,
      data_stream_tx_ack  => uart_tx_ack,
      data_stream_rx      => uart_rx_data,
      data_stream_rx_stb  => uart_rx_stb,
      tx                  => tx,
      rx                  => rx
    );

  -----------------------------------------------------------------------------
  -- FIFO
  -----------------------------------------------------------------------------
  fifo_buffer: entity work.fifo
    generic map (
      FIFO_WIDTH => 8,
      FIFO_DEPTH => BUFFER_DEPTH
    )
    port map (
      clk        => clk,
      rst        => rst,
      write_data => fifo_data_in,
      write_en   => fifo_write_en,
      read_data  => fifo_data_out,
      read_en    => fifo_read_en,
      full       => fifo_full,
      empty      => fifo_empty,
      size       => open,
      overflow   => open,
      underflow  => open
    );

  -----------------------------------------------------------------------------
  -- Top-level syncs and TX pin
  -----------------------------------------------------------------------------
  process (clk)
  begin
    if rising_edge(clk) then
      -- 2-FF sync for asynchronous reset_n and RX input
      rst_sync <= not rst_n;
      rst      <= rst_sync;

      rx_sync  <= uart_rxd;
      rx       <= rx_sync;

      uart_txd <= tx;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- RX path: push each received byte into FIFO (one clock strobe)
  -----------------------------------------------------------------------------
  process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        fifo_write_en <= '0';
        fifo_data_in  <= (others => '0');
      else
        fifo_write_en <= '0';              -- default
        if (uart_rx_stb = '1') and (fifo_full = '0') then
          fifo_data_in  <= uart_rx_data;   -- capture received byte
          fifo_write_en <= '1';            -- write one cycle into FIFO
        end if;
      end if;
    end if;
  end process;

  -----------------------------------------------------------------------------
  -- TX pipeline: safe handoff from FIFO to UART TX
  -- IDLE  : wait for data in FIFO and UART idle
  -- READ1 : assert FIFO read (registered read kicks off)
  -- LATCH : one extra cycle; fifo_data_out is now valid -> latch into tx_byte
  -- SEND  : assert uart_tx_stb with tx_byte; hold until uart_tx_ack
  -----------------------------------------------------------------------------
  process (clk)
  begin
    if rising_edge(clk) then
      if rst = '1' then
        fifo_read_en <= '0';
        uart_tx_stb  <= '0';
        uart_tx_data <= (others => '0');
        tx_byte      <= (others => '0');
        txp_state    <= IDLE;
      else
        -- default deassertions
        fifo_read_en <= '0';

        -- Deassert TX strobe as soon as the UART core acknowledges it.
        -- (UART samples the strobe on its baud tick in the start state.)
        if uart_tx_ack = '1' then
          uart_tx_stb <= '0';
        end if;

        case txp_state is
          when IDLE =>
            -- Start a read when there is data and TX strobe is not active
            if (fifo_empty = '0') and (uart_tx_stb = '0') then
              fifo_read_en <= '1';   -- kick the registered read
              txp_state    <= READ1; -- wait 1 cycle
            end if;

          when READ1 =>
            -- The read happened on this rising edge; wait one more cycle
            -- so fifo_data_out becomes valid (registered output).
            txp_state    <= LATCH;

          when LATCH =>
            -- Now fifo_data_out is guaranteed valid: latch it and request TX
            tx_byte      <= fifo_data_out;
            uart_tx_data <= fifo_data_out;
            uart_tx_stb  <= '1';     -- keep asserted until uart_tx_ack
            txp_state    <= SEND;

          when SEND =>
            -- Stay here while UART is accepting/starting the frame.
            -- We return to IDLE when ack arrives (strobe is cleared above),
            -- allowing immediate fetch of the next FIFO word (no RX dependency).
            if uart_tx_ack = '1' then
              txp_state <= IDLE;
            end if;

          when others =>
            txp_state <= IDLE;
        end case;
      end if;
    end if;
  end process;

end architecture;
