library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity uart_tb is
end entity;

architecture sim of uart_tb is

  -- Baud rate for UART simulation
  constant BAUD_RATE : positive := 115200;
  -- Clock frequency for simulation (50 MHz)
  constant CLK_FREQ  : positive := 50_000_000;
  -- Clock period for 50 MHz
  constant CLK_PERIOD : time := 20 ns;
  
  -- UART TX idle state is high, BAUD_PERIOD is one bit time
  constant BAUD_PERIOD : time := 1 sec / BAUD_RATE;

  -- Clock signal for DUT
  signal clk                 : std_logic := '0';
  -- Active-high synchronous reset
  signal rst                 : std_logic := '0';
  -- Parallel data byte to send via UART
  signal data_stream_tx      : std_logic_vector(7 downto 0) := (others => '0');
  -- Strobe to start transmission of data_stream_tx
  signal data_stream_tx_stb  : std_logic := '0';
  -- Acknowledge from DUT that TX data was accepted
  signal data_stream_tx_ack  : std_logic;
  -- Parallel data byte received from UART
  signal data_stream_rx      : std_logic_vector(7 downto 0);
  -- Strobe indicating valid received byte
  signal data_stream_rx_stb  : std_logic;
  -- UART TX serial output line
  signal tx                  : std_logic;
  -- UART RX serial input line (idle high)
  signal rx                  : std_logic := '1';

  -----------------------------------------------------------------------------
  -- Hex conversion helper (converts std_logic_vector to hex string)
  -----------------------------------------------------------------------------
  function to_hex_str(
    v : std_logic_vector
  ) return string is
    constant N : integer := (v'length + 3) / 4;
    variable res : string(1 to N);
    variable u   : unsigned(v'length-1 downto 0) := unsigned(v);
    variable idx : integer := 0;
    variable nib : integer;
  begin
    for i in 0 to N-1 loop
      idx := i*4;
      if idx+3 <= v'length-1 then
        nib := to_integer(u(idx+3 downto idx));
      else
        nib := 0;
      end if;
      case nib is
        when  0 => res(N-i) := '0';
        when  1 => res(N-i) := '1';
        when  2 => res(N-i) := '2';
        when  3 => res(N-i) := '3';
        when  4 => res(N-i) := '4';
        when  5 => res(N-i) := '5';
        when  6 => res(N-i) := '6';
        when  7 => res(N-i) := '7';
        when  8 => res(N-i) := '8';
        when  9 => res(N-i) := '9';
        when 10 => res(N-i) := 'A';
        when 11 => res(N-i) := 'B';
        when 12 => res(N-i) := 'C';
        when 13 => res(N-i) := 'D';
        when 14 => res(N-i) := 'E';
        when others => res(N-i) := 'F';
      end case;
    end loop;
    return res;
  end function;
  
  -----------------------------------------------------------------------------
  -- Sends a UART frame (start bit, 8 data bits, stop bit) onto the RX line
  -----------------------------------------------------------------------------
  procedure send_rx_byte(
    signal rx_line : out std_logic;
    val : std_logic_vector(7 downto 0)
  ) is
    variable i : integer;
  begin
    -- Send start bit (logic 0)
    rx_line <= '0';
    wait for BAUD_PERIOD;

    -- Send 8 data bits, LSB first
    for i in 0 to 7 loop
      rx_line <= val(i);
      wait for BAUD_PERIOD;
    end loop;

    -- Send stop bit (logic 1)
    rx_line <= '1';
    wait for BAUD_PERIOD;
  end procedure;

  -----------------------------------------------------------------------------
  -- Captures one UART frame from the TX line and checks it
  -----------------------------------------------------------------------------
  procedure check_tx_byte(
    expected : std_logic_vector(7 downto 0)
  ) is
    variable received : std_logic_vector(7 downto 0) := (others => '0');
    variable i        : integer;
  begin
    -- wait until tx = '0'; -- wait for start bit
    wait for BAUD_PERIOD + BAUD_PERIOD/2; -- center sample first bit
    for i in 0 to 7 loop
      received(i) := tx; -- sample each data bit
      wait for BAUD_PERIOD;
    end loop;
    assert tx = '1' report "TX stop bit not high" severity error;
    wait for BAUD_PERIOD;
    assert received = expected
      report "TX data mismatch: expected 0x" & to_hex_str(expected) &
             ", got 0x" & to_hex_str(received)
      severity error;
  end procedure;

begin

  -----------------------------------------------------------------------------
  -- DUT: UART
  -----------------------------------------------------------------------------
  dut: entity work.uart
    generic map (
      BAUD_RATE => BAUD_RATE,
      CLK_FREQ  => CLK_FREQ
    )
    port map (
      clk                => clk,
      rst                => rst,
      data_stream_tx     => data_stream_tx,
      data_stream_tx_stb => data_stream_tx_stb,
      data_stream_tx_ack => data_stream_tx_ack,
      data_stream_rx     => data_stream_rx,
      data_stream_rx_stb => data_stream_rx_stb,
      tx                 => tx,
      rx                 => rx
    );

  -----------------------------------------------------------------------------
  -- Clock process: generates 50 MHz system clock
  -----------------------------------------------------------------------------
  clk_proc : process
  begin
    while true loop
      clk <= '0';
      wait for CLK_PERIOD/2;
      clk <= '1';
      wait for CLK_PERIOD/2;
    end loop;
  end process;

  -----------------------------------------------------------------------------
  -- Main stimulus process: runs all test cases
  -----------------------------------------------------------------------------
  stim_proc: process
  
    ---------------------------------------------------------------------------
    -- Helper to apply synchronous reset to DUT
    ---------------------------------------------------------------------------
    procedure reset is
    begin
      data_stream_tx <= x"00";
      rst <= '1';
      wait for 2 * CLK_PERIOD;
      rst <= '0';
      wait for CLK_PERIOD;
    end procedure;

  begin
    ---------------------------------------------------------------------------
    -- Test 1: Reset and idle line check
    ---------------------------------------------------------------------------
    report "Test 1: Reset and idle line";
    
    reset;
    
    assert data_stream_tx_ack = '0' 
      report "TX ack should be 0 after reset" 
      severity error;

    assert data_stream_rx_stb = '0' 
      report "RX strobe should be 0 after reset" 
      severity error;

    assert tx = '1' 
      report "TX line should be idle high after reset" 
      severity error;
    
    wait for 10 * CLK_PERIOD;

    ------------------------------------------------------------------------
    -- Test 2: Simple transmit test
    ------------------------------------------------------------------------
    report "Test 2: Transmit one byte";

    reset;

    data_stream_tx <= x"5A";
    data_stream_tx_stb <= '1';

    wait until data_stream_tx_ack = '1';
    
    data_stream_tx_stb <= '0';
    check_tx_byte(x"5A");
    
    wait for 10 * CLK_PERIOD;

    ---------------------------------------------------------------------------
    -- Test 3: Simple receive test
    ---------------------------------------------------------------------------
    report "Test 3: Receive one byte";
    reset;

    send_rx_byte(rx, x"A5");

    assert data_stream_rx = x"A5"
     report "RX data mismatch: expected 0xA5, got 0x" & to_hex_str(data_stream_rx) 
     severity error;

    wait for 10 * CLK_PERIOD;

    ---------------------------------------------------------------------------
    -- Test 4: Transmit bytes
    ---------------------------------------------------------------------------
    report "Test 4: Transmit bytes";
    reset;

    data_stream_tx <= x"FF";
    data_stream_tx_stb <= '1';

    wait until data_stream_tx_ack = '1';
    
    data_stream_tx_stb <= '0';
    
    check_tx_byte(x"FF");

    data_stream_tx <= x"00";
    data_stream_tx_stb <= '1';

    wait until data_stream_tx_ack = '1'; 
    
    data_stream_tx_stb <= '0';
    
    check_tx_byte(x"00");

    wait for 10 * CLK_PERIOD;

    ---------------------------------------------------------------------------
    -- Test 6: Reset during transmission
    ---------------------------------------------------------------------------
    report "Test 6: Reset during transmit";
    reset;
    
    data_stream_tx <= x"AA"; data_stream_tx_stb <= '1';
    
    wait until data_stream_tx_ack = '1'; data_stream_tx_stb <= '0';
    
    wait for BAUD_PERIOD * 4;
    
    rst <= '1'; wait for 5 * CLK_PERIOD; rst <= '0';
    
    wait for BAUD_PERIOD;
    
    assert tx = '1' report "TX line should be idle after reset during transmit" severity error;

    wait for 10 * CLK_PERIOD;
    
    report "All UART tests completed successfully!";
    wait;
  end process;

end architecture;
