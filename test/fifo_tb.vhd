library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity fifo_tb is
end entity;

architecture sim of fifo_tb is

  constant FIFO_WIDTH : positive := 8;
  constant FIFO_DEPTH : positive := 8;

  -- DUT signals
  signal clk        : std_logic := '0';
  signal rst        : std_logic := '0';
  signal write_data : std_logic_vector(FIFO_WIDTH-1 downto 0) := (others => '0');
  signal read_data  : std_logic_vector(FIFO_WIDTH-1 downto 0);
  signal write_en   : std_logic := '0';
  signal read_en    : std_logic := '0';
  signal full       : std_logic;
  signal empty      : std_logic;
  signal size       : std_logic_vector(integer(ceil(log2(real(FIFO_DEPTH)))) downto 0);
  signal overflow   : std_logic;
  signal underflow  : std_logic;

  -- Clock period
  constant CLK_PERIOD : time := 10 ns;

begin

  --------------------------------------------------------------------------
  -- Instantiate the FIFO
  --------------------------------------------------------------------------
  dut: entity work.fifo
    generic map (
      FIFO_WIDTH => FIFO_WIDTH,
      FIFO_DEPTH => FIFO_DEPTH
    )
    port map (
      clk        => clk,
      rst        => rst,
      write_data => write_data,
      read_data  => read_data,
      write_en   => write_en,
      read_en    => read_en,
      full       => full,
      empty      => empty,
      size       => size,
      overflow   => overflow,
      underflow  => underflow
    );

  --------------------------------------------------------------------------
  -- Clock generator
  --------------------------------------------------------------------------
  clk_proc: process
  begin
    while true loop
      clk <= '0';
      wait for CLK_PERIOD/2;
      clk <= '1';
      wait for CLK_PERIOD/2;
    end loop;
  end process;

  --------------------------------------------------------------------------
  -- Stimulus process
  --------------------------------------------------------------------------
  stim_proc: process

    procedure write_byte(val : integer) is
    begin
      write_data <= std_logic_vector(to_unsigned(val, FIFO_WIDTH));
      write_en   <= '1';
      wait for CLK_PERIOD;
      write_en   <= '0';
      wait for CLK_PERIOD;
    end procedure;

    procedure read_byte(expected : integer) is
    begin
      read_en <= '1';
      wait for CLK_PERIOD;
      read_en <= '0';
      wait for CLK_PERIOD;

      assert to_integer(unsigned(read_data)) = expected
        report "Read data mismatch! Expected " & integer'image(expected) &
               ", got " & integer'image(to_integer(unsigned(read_data)))
        severity error;
    end procedure;

    procedure reset is
    begin
      -- Apply reset
      rst <= '1';
      wait for 2 * CLK_PERIOD;
      rst <= '0';
      wait for CLK_PERIOD;
    end procedure;

  begin
    reset;

    ----------------------------------------------------------------------
    -- TEST 1: Basic write and read
    ----------------------------------------------------------------------
    report "TEST 1: Basic write/read";
    write_byte(10);
    write_byte(20);
    read_byte(10);
    read_byte(20);
    assert empty = '1' report "FIFO not empty after all reads" severity error;
    assert overflow = '0' report "Unexpected overflow flag" severity error;
    assert underflow = '0' report "Unexpected underflow flag" severity error;

    reset;
    wait for 10 * CLK_PERIOD;

    ----------------------------------------------------------------------
    -- TEST 2: Fill until full
    ----------------------------------------------------------------------
    report "TEST 2: Fill until full";
    for i in 1 to FIFO_DEPTH loop
      write_byte(i);
    end loop;
    assert full = '1' report "FIFO not full when expected" severity error;
    assert overflow = '0' report "Unexpected overflow flag" severity error;
    
    wait for 10 * CLK_PERIOD;

    ----------------------------------------------------------------------
    -- TEST 3: Read until empty
    ----------------------------------------------------------------------
    report "TEST 3: Read until empty";
    for i in 1 to FIFO_DEPTH loop
      read_byte(i);
    end loop;
    assert empty = '1' report "FIFO not empty after draining" severity error;
    assert underflow = '0' report "Unexpected underflow flag" severity error;

    reset;
    wait for 10 * CLK_PERIOD;

    ----------------------------------------------------------------------
    -- TEST 4: Simultaneous read/write
    ----------------------------------------------------------------------
    report "TEST 4: Simultaneous read/write";
    -- Fill half FIFO
    for i in 1 to FIFO_DEPTH/2 loop
      write_byte(100+i);
    end loop;

    -- Simultaneously read and write, FIFO size should remain stable
    for i in 1 to FIFO_DEPTH/2 loop
      write_data <= std_logic_vector(to_unsigned(200+i, FIFO_WIDTH));
      write_en   <= '1';
      read_en    <= '1';
      wait for CLK_PERIOD;
      write_en   <= '0';
      read_en    <= '0';
      wait for CLK_PERIOD;
  
      -- The data read should be 100+i (old data)
      assert to_integer(unsigned(read_data)) = (100 + i)
        report "Mismatch in simultaneous read/write at iteration " & integer'image(i)
        severity error;
    end loop;

    assert full = '0' report "FIFO should not be full after simultaneous operations" severity error;
    assert empty = '0' report "FIFO should not be empty after simultaneous operations" severity error;

    reset;
    wait for 10 * CLK_PERIOD;

    ----------------------------------------------------------------------
    -- TEST 5: Wrap-around behavior
    ----------------------------------------------------------------------
    report "TEST 5: Wrap-around";
    -- Fill completely
    for i in 1 to FIFO_DEPTH loop
      write_byte(50+i);
    end loop;
    assert full = '1' report "FIFO not full when expected" severity error;

    -- Read a few elements to advance pointer and wrap around
    for i in 1 to FIFO_DEPTH/2 loop
      read_byte(50+i);
    end loop;
    assert empty = '0' report "FIFO should not be empty yet" severity error;

    -- Write again to wrap around the write pointer
    for i in 1 to FIFO_DEPTH/2 loop
      write_byte(150+i);
    end loop;
    assert full = '1' report "FIFO should be full after wrap-around writes" severity error;

    -- Read the rest to ensure correct sequence
    for i in (FIFO_DEPTH/2+1) to FIFO_DEPTH loop
      read_byte(50+i);
    end loop;
    for i in 1 to FIFO_DEPTH/2 loop
      read_byte(150+i);
    end loop;
    assert empty = '1' report "FIFO should be empty after final reads" severity error;

    reset;
    wait for 10 * CLK_PERIOD;

    ----------------------------------------------------------------------
    -- TEST 6: Edge case - Write when full
    ----------------------------------------------------------------------
    report "TEST 6: Write when full (should assert overflow and ignore)";
    for i in 1 to FIFO_DEPTH loop
      write_byte(5+i);
    end loop;
    -- Try writing one more (should be ignored and overflow asserted)
    write_data <= std_logic_vector(to_unsigned(999, FIFO_WIDTH));
    write_en <= '1';
    wait for CLK_PERIOD;
    write_en <= '0';
    wait for CLK_PERIOD;
    assert overflow = '1' report "Overflow flag not asserted on write to full FIFO" severity error;
    assert full = '1' report "FIFO should still be full" severity error;

    ----------------------------------------------------------------------
    -- TEST 7: Edge case - Read when empty
    ----------------------------------------------------------------------
    report "TEST 7: Read when empty (should assert underflow and ignore)";
    for i in 1 to FIFO_DEPTH loop
      read_byte(5+i);
    end loop;
    assert empty = '1' report "FIFO should be empty before extra read" severity error;

    -- Extra read, should assert underflow
    read_en <= '1';
    wait for CLK_PERIOD;
    read_en <= '0';
    wait for CLK_PERIOD;
    assert underflow = '1' report "Underflow flag not asserted on read from empty FIFO" severity error;

    report "All tests completed successfully!";
    wait;
  end process;

end architecture;
