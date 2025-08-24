library ieee;
use ieee.std_logic_1164.all;
use ieee.numeric_std.all;
use ieee.math_real.all;

entity fifo is
  generic (
    FIFO_WIDTH : positive := 32;  -- Width of each data word
    FIFO_DEPTH : positive := 1024 -- FIFO depth, must be a power of 2 for pointer wrap-around
  );
  port (
    clk         : in  std_logic;  -- System clock
    rst         : in  std_logic;  -- Synchronous reset, active-high
    write_data  : in  std_logic_vector(FIFO_WIDTH - 1 downto 0);  -- Data to write
    read_data   : out std_logic_vector(FIFO_WIDTH - 1 downto 0);  -- Data read from FIFO
    write_en    : in  std_logic;  -- Write enable signal
    read_en     : in  std_logic;  -- Read enable signal
    full        : out std_logic;  -- FIFO full flag, '1' when FIFO is full
    empty       : out std_logic;  -- FIFO empty flag, '1' when FIFO is empty
    size        : out std_logic_vector(
                    integer(ceil(log2(real(FIFO_DEPTH)))) downto 0
                  );              -- Current number of entries in FIFO (usage)
    overflow    : out std_logic;  -- Write attempted when FIFO is full
    underflow   : out std_logic   -- Read attempted when FIFO is empty
  );
end entity;

architecture rtl of fifo is
  -- Number of bits needed to index FIFO entries
  constant PTR_WIDTH    : integer := integer(ceil(log2(real(FIFO_DEPTH))));

  -- FIFO storage array type, holds FIFO_DEPTH entries of FIFO_WIDTH bits each
  type fifo_array_t     is array (0 to FIFO_DEPTH-1) of std_logic_vector(FIFO_WIDTH-1 downto 0);

  -- Internal signals
  signal fifo_storage   : fifo_array_t := (others => (others => '0')); -- FIFO data memory
  signal read_pointer   : unsigned(PTR_WIDTH-1 downto 0) := (others => '0'); -- Points to next word to read
  signal write_pointer  : unsigned(PTR_WIDTH-1 downto 0) := (others => '0'); -- Points to next word to write

  -- Usage count tracks number of stored entries in FIFO; 
  -- Has one extra bit (PTR_WIDTH downto 0) to represent the full range from 0 to FIFO_DEPTH
  signal usage          : unsigned(PTR_WIDTH downto 0)   := (others => '0');

  signal read_data_reg  : std_logic_vector(FIFO_WIDTH-1 downto 0) := (others => '0'); -- Registered output data

  -- Flags for overflow and underflow error conditions
  signal overflow_flag  : std_logic := '0';
  signal underflow_flag : std_logic := '0';

begin
  -- FIFO is full when usage count equals FIFO_DEPTH
  full      <= '1' when usage = FIFO_DEPTH else '0';

  -- FIFO is empty when usage is zero
  empty     <= '1' when usage = 0 else '0';

  -- Output current FIFO usage as std_logic_vector
  size      <= std_logic_vector(usage);

  -- Registered FIFO output data
  read_data <= read_data_reg;

  -- Overflow and underflow flags indicate invalid write/read attempts
  overflow  <= overflow_flag;
  underflow <= underflow_flag;

  ----------------------------------------------------------------------------
  -- Main synchronous process: handles read/write operations and flags
  ----------------------------------------------------------------------------
  process(clk)
    variable do_write : boolean;
    variable do_read  : boolean;
  begin
    if rising_edge(clk) then
      if rst = '1' then
        read_pointer    <= (others => '0');
        write_pointer   <= (others => '0');
        usage           <= (others => '0');
        read_data_reg   <= (others => '0');
        overflow_flag   <= '0';
        underflow_flag  <= '0';
      else
        do_write := (write_en = '1') and (usage < FIFO_DEPTH);
        do_read  := (read_en  = '1') and (usage > 0);

        -- Reset error flags each clock cycle
        overflow_flag  <= '0';
        underflow_flag <= '0';

        -- Perform write operation if valid
        if do_write then
          -- Write input data to FIFO at current write_pointer location
          fifo_storage(to_integer(write_pointer)) <= write_data;

          -- Increment write pointer with automatic wrap-around (due to unsigned arithmetic)
          write_pointer <= write_pointer + 1;

          -- Increment usage count only if no simultaneous read
          if not do_read then
            usage <= usage + 1;
          end if;

        -- Flag overflow if write requested but FIFO is full
        elsif (write_en = '1') and not do_write then
          overflow_flag <= '1';
        end if;

        -- Perform read operation if valid
        if do_read then
          -- Output data at current read_pointer location (registered)
          read_data_reg <= fifo_storage(to_integer(read_pointer));

          -- Increment read pointer with wrap-around
          read_pointer  <= read_pointer + 1;

          -- Decrement usage count only if no simultaneous write
          if not do_write then
            usage <= usage - 1;
          end if;

        -- Flag underflow if read requested but FIFO is empty
        elsif (read_en = '1') and not do_read then
          underflow_flag <= '1';
        end if;
      end if;
    end if;
  end process;

end architecture;