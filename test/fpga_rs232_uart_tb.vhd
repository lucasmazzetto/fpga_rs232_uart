library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;

entity fpga_rs232_uart_tb is
end fpga_rs232_uart_tb;

architecture sim of fpga_rs232_uart_tb is
  -------------------------------------------------------------------
  -- Parameters
  -------------------------------------------------------------------
  constant CLK_FREQ    : integer := 50_000_000; -- 50 MHz
  constant BAUD_RATE   : integer := 115200;
  constant CLK_PERIOD  : time    := 1 sec / CLK_FREQ;
  constant BAUD_PERIOD : time    := 1 sec / BAUD_RATE;

  -------------------------------------------------------------------
  -- DUT signals
  -------------------------------------------------------------------
  signal clk      : std_logic := '0';
  signal rst_n    : std_logic := '1';
  signal uart_rxd : std_logic := '1'; -- idle = '1'
  signal uart_txd : std_logic;

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

begin

  clk_proc : process
  begin
    while true loop
      clk <= '0';
      wait for CLK_PERIOD/2;
      clk <= '1';
      wait for CLK_PERIOD/2;
    end loop;
  end process;

  -------------------------------------------------------------------
  -- DUT instantiation
  -------------------------------------------------------------------
  uut: entity work.fpga_rs232_uart
    generic map (
      CLK_FREQ  => CLK_FREQ,
      BAUD_RATE => BAUD_RATE
    )
    port map (
      clk      => clk,
      rst_n    => rst_n,
      uart_rxd => uart_rxd,
      uart_txd => uart_txd
    );

  -------------------------------------------------------------------
  -- Stimulus process
  -------------------------------------------------------------------
  process
  begin
    -- rst_n <= '0';
    -- wait for 250 ns;
    -- rst_n <= '1';
    wait for 100 us;
    
    report "Test: sending bytes to UART RXD";

    send_rx_byte(uart_rxd, x"AA");
    wait for 200 us;

    send_rx_byte(uart_rxd, x"BB");
    wait for 200 us;

    send_rx_byte(uart_rxd, x"CC");
    wait for 200 us;

    report "All bytes sent";

    wait for 200 us;

    report "All UART tests completed successfully!";
    
    wait;
  end process;

end architecture;
