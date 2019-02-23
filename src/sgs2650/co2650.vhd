---------------------------------------------------------------------------------
-- Games consoles with Signetics 2650 CPU

-- Video 2636 PVI. Interton VC4000 & clones
-- Video 2637 UVI. Emerson Arcadia & clones

---------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.numeric_std.all;
USE IEEE.std_logic_unsigned.ALL;

USE std.textio.ALL;

LIBRARY work;
USE work.base_pack.ALL;

ENTITY emu IS
  PORT (
    -- Master input clock
    clk_50m          : IN    std_logic;

    -- Async reset from top-level module. Can be used as initial reset.
    reset            : IN    std_logic;

    -- Must be passed to hps_io module
    hps_bus          : INOUT std_logic_vector(44 DOWNTO 0);
    
    -- Base video clock. Usually equals to CLK_SYS.
    clk_video        : OUT   std_logic;

    -- Multiple resolutions are supported using different CE_PIXEL rates.
    -- Must be based on CLK_VIDEO
    ce_pixel         : OUT   std_logic;

    -- Video aspect ratio for HDMI. Most retro systems have ratio 4:3.
    video_arx        : OUT   std_logic_vector(7 DOWNTO 0);
    video_ary        : OUT   std_logic_vector(7 DOWNTO 0);

    -- VGA
    vga_r            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_g            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_b            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_hs           : OUT   std_logic; -- positive pulse!
    vga_vs           : OUT   std_logic; -- positive pulse!
    vga_de           : OUT   std_logic; -- = not (VBlank or HBlank)
    vga_f1           : OUT   std_logic;
    vga_sl           : OUT   std_logic_vector(1 DOWNTO 0);
    
    -- LED
    led_user         : OUT   std_logic; -- 1 - ON, 0 - OFF.

    -- b[1]: 0 - LED status is system status ORed with b[0]
    --       1 - LED status is controled solely by b[0]
    -- hint: supply 2'b00 to let the system control the LED.
    led_power        : OUT   std_logic_vector(1 DOWNTO 0);
    led_disk         : OUT   std_logic_vector(1 DOWNTO 0);

    -- AUDIO
    audio_l          : OUT   std_logic_vector(15 DOWNTO 0);
    audio_r          : OUT   std_logic_vector(15 DOWNTO 0);
    audio_s          : OUT   std_logic; -- 1 = signed audio, 0 = unsigned
    -- 0 - no mix, 1 - 25%, 2 - 50%, 3 - 100% (mono)
    audio_mix        : OUT   std_logic_vector(1 DOWNTO 0);
    
    tape_in          : IN    std_logic;
    
    -- SD-SPI
    sd_sck           : OUT   std_logic := 'Z';
    sd_mosi          : OUT   std_logic := 'Z';
    sd_miso          : IN    std_logic;
    sd_cs            : OUT   std_logic := 'Z';
    sd_cd            : IN    std_logic;

    -- High latency DDR3 RAM interface
    -- Use for non-critical time purposes
    ddram_clk        : OUT   std_logic;
    ddram_busy       : IN    std_logic;
    ddram_burstcnt   : OUT   std_logic_vector(7 DOWNTO 0);
    ddram_addr       : OUT   std_logic_vector(28 DOWNTO 0);
    ddram_dout       : IN    std_logic_vector(63 DOWNTO 0);
    ddram_dout_ready : IN    std_logic;
    ddram_rd         : OUT   std_logic;
    ddram_din        : OUT   std_logic_vector(63 DOWNTO 0);
    ddram_be         : OUT   std_logic_vector(7 DOWNTO 0);
    ddram_we         : OUT   std_logic;

    -- SDRAM interface with lower latency
    sdram_clk        : OUT   std_logic;
    sdram_cke        : OUT   std_logic;
    sdram_a          : OUT   std_logic_vector(12 DOWNTO 0);
    sdram_ba         : OUT   std_logic_vector(1 DOWNTO 0);
    sdram_dq         : INOUT std_logic_vector(15 DOWNTO 0);
    sdram_dqml       : OUT   std_logic;
    sdram_dqmh       : OUT   std_logic;
    sdram_ncs        : OUT   std_logic;
    sdram_ncas       : OUT   std_logic;
    sdram_nras       : OUT   std_logic;
    sdram_nwe        : OUT   std_logic);
END emu;

ARCHITECTURE struct OF emu IS

  CONSTANT CDIV : natural := 12 * 8;
  
  COMPONENT pll IS
    PORT (
      refclk   : in  std_logic; -- clk
      rst      : in  std_logic; -- reset
      outclk_0 : out std_logic; -- clk
      outclk_1 : out std_logic; -- clk
      locked   : out std_logic  -- export
      );
  END COMPONENT pll;
  
  CONSTANT CONF_STR : string := 
    "CO2650;;" &
    "F,BIN,Load Cartridge;" &
    "O0,Video standard,PAL,NTSC;" &
    "O1,Mode,Interton,Arcadia;" &
    "O3,Swap Joystick,Off,On;" &
    "O4,Swap Joystick XY,Off,On;" &
    "O2,Overlay,Off,On;" &
    "R0,Reset & Detach cartridge;" &
    "J,Start,Select,B1,B2,B3,B4,B5,B6,B7,B8,B9;" &
    "V1.0";

  FUNCTION to_slv(s: string) return std_logic_vector is 
    CONSTANT ss : string(1 to s'length) := s; 
    VARIABLE rval : std_logic_vector(1 to 8 * s'length); 
    VARIABLE p : integer; 
    VARIABLE c : integer; 
  BEGIN
    FOR i in ss'range LOOP
      p := 8 * i;
      c := character'pos(ss(i));
      rval(p - 7 to p) := std_logic_vector(to_unsigned(c,8)); 
    END LOOP;
    RETURN rval;
  END FUNCTION; 

  COMPONENT hps_io
    GENERIC (
      STRLEN : integer := 0;
      PS2DIV : integer := 1000;
      WIDE   : integer := 0; --  8bits download bus
      VDNUM  : integer := 1;
      PS2WE  : integer := 0);
    PORT (
      clk_sys           : IN  std_logic;
      hps_bus           : INOUT std_logic_vector(44 DOWNTO 0);

      conf_str          : IN  std_logic_vector(8*STRLEN-1 DOWNTO 0);

      buttons           : OUT std_logic_vector(1 DOWNTO 0);
      forced_scandoubler: OUT std_logic;

      joystick_0        : OUT std_logic_vector(15 DOWNTO 0);
      joystick_1        : OUT std_logic_vector(15 DOWNTO 0);
      joystick_analog_0 : OUT std_logic_vector(15 DOWNTO 0);
      joystick_analog_1 : OUT std_logic_vector(15 DOWNTO 0);
      status            : OUT std_logic_vector(31 DOWNTO 0);

      sd_lba            : IN  std_logic_vector(31 DOWNTO 0);
      sd_rd             : IN  std_logic;
      sd_wr             : IN  std_logic;
      sd_ack            : OUT std_logic;
      sd_conf           : IN  std_logic;
      sd_ack_conf       : OUT std_logic;

      sd_buff_addr      : OUT std_logic_vector(8 DOWNTO 0);
      sd_buff_dout      : OUT std_logic_vector(7 DOWNTO 0);
      sd_buff_din       : IN  std_logic_vector(7 DOWNTO 0);
      sd_buff_wr        : OUT std_logic;

      img_mounted       : OUT std_logic;
      img_size          : OUT std_logic_vector(63 DOWNTO 0);
      img_readonly      : OUT std_logic;

      ioctl_download    : OUT std_logic;
      ioctl_index       : OUT std_logic_vector(7 DOWNTO 0);
      ioctl_wr          : OUT std_logic;
      ioctl_addr        : OUT std_logic_vector(24 DOWNTO 0);
      ioctl_dout        : OUT std_logic_vector(7 DOWNTO 0);
      ioctl_wait        : IN  std_logic;
      
      rtc               : OUT std_logic_vector(64 DOWNTO 0);
      timestamp         : OUT std_logic_vector(32 DOWNTO 0);

      ps2_kbd_clk_out   : OUT std_logic;
      ps2_kbd_data_out  : OUT std_logic;
      ps2_kbd_clk_in    : IN  std_logic;
      ps2_kbd_data_in   : IN  std_logic;

      ps2_kbd_led_use   : IN  std_logic_vector(2 DOWNTO 0);
      ps2_kbd_led_status: IN  std_logic_vector(2 DOWNTO 0);

      ps2_mouse_clk_out : OUT std_logic;
      ps2_mouse_data_out: OUT std_logic;
      ps2_mouse_clk_in  : IN  std_logic;
      ps2_mouse_data_in : IN  std_logic;

      ps2_key           : OUT std_logic_vector(10 DOWNTO 0);
      ps2_mouse         : OUT std_logic_vector(24 DOWNTO 0));
  END COMPONENT hps_io;

  SIGNAL buttons          : std_logic_vector(1 DOWNTO 0);
  SIGNAL status           : std_logic_vector(31 DOWNTO 0);
  SIGNAL joystick_0       : std_logic_vector(15 DOWNTO 0);
  SIGNAL joystick_1       : std_logic_vector(15 DOWNTO 0);
  SIGNAL joystick_analog_0 : std_logic_vector(15 DOWNTO 0);
  SIGNAL joystick_analog_1 : std_logic_vector(15 DOWNTO 0);
 
  SIGNAL ioctl_download   : std_logic;
  SIGNAL ioctl_index      : std_logic_vector(7 DOWNTO 0);
  SIGNAL ioctl_wr         : std_logic;
  SIGNAL ioctl_addr       : std_logic_vector(24 DOWNTO 0);
  SIGNAL ioctl_dout       : std_logic_vector(7 DOWNTO 0);
  SIGNAL ioctl_wait       : std_logic :='0';
  
  SIGNAL ntsc_pal,arca,swap,swapxy : std_logic;
  
  SIGNAL ps2_key,ps2_key_delay : std_logic_vector(10 DOWNTO 0);
  
  SIGNAL key_0, key_1 , key_2 , key_3  : std_logic;
  SIGNAL key_4, key_5 , key_6 , key_7  : std_logic;
  SIGNAL key_8, key_9 , key_a , key_b  : std_logic;
  SIGNAL key_c, key_d , key_e , key_f  : std_logic;

  SIGNAL key_rc   ,key_wc  ,key_bp ,key_pc  : std_logic;
  SIGNAL key_minus,key_plus,key_reg,key_mem : std_logic;
  SIGNAL key_select , key_start : std_logic;

  --------------------------------------
  SIGNAL in_keypad1_1, in_keypad1_2, in_keypad1_3 : unsigned(7 DOWNTO 0);
  SIGNAL in_keypad2_1, in_keypad2_2, in_keypad2_3 : unsigned(7 DOWNTO 0);
  SIGNAL in_keypanel,  in_volnoise : unsigned(7 DOWNTO 0);
  
  --------------------------------------
  SIGNAL ac_keypad1_1, ac_keypad1_2, ac_keypad1_3 : unsigned(7 DOWNTO 0);
  SIGNAL ac_keypad2_1, ac_keypad2_2, ac_keypad2_3 : unsigned(7 DOWNTO 0);
  SIGNAL ac_keypanel : unsigned(7 DOWNTO 0);
  
  --------------------------------------
  SIGNAL vol : unsigned(1 DOWNTO 0);
  SIGNAL icol,explo,noise : std_logic;
  SIGNAL sound,in_sound,ac_sound : unsigned(7 DOWNTO 0);
  SIGNAL pot1,pot2 : unsigned(7 DOWNTO 0);
  SIGNAL potl_a,potl_b,potr_a,potr_b : unsigned(7 DOWNTO 0);
  SIGNAL potl_v,potl_h,potr_v,potr_h : unsigned(7 DOWNTO 0);
  
  SIGNAL clksys,clksys_ntsc,clksys_pal,pll_locked : std_logic;
  
  SIGNAL tick_cpu_cpt : natural RANGE 0 TO CDIV-1;
  SIGNAL tick_cpu : std_logic;
  
  SIGNAL ad,ad_delay,ad_rom : unsigned(14 DOWNTO 0);
  SIGNAL dr,dw,dr_pvi,dr_uvi,dr_rom,dr_in_key,dr_ac_key : unsigned(7 DOWNTO 0);
  SIGNAL req,req_pvi,ack,ack_pvi,req_uvi,ack_uvi : std_logic;
  SIGNAL int, int_pvi,intack,creset : std_logic;
  SIGNAL sense,flag : std_logic;
  SIGNAL mio,ene,dc,wr : std_logic;
  SIGNAL ph : unsigned(1 DOWNTO 0);
  SIGNAL ivec : unsigned(7 DOWNTO 0);
  
  SIGNAL reset_na : std_logic;
  SIGNAL w_d : unsigned(7 DOWNTO 0);
  SIGNAL w_a : unsigned(12 DOWNTO 0);
  SIGNAL w_wr : std_logic;
  TYPE arr_cart IS ARRAY(natural RANGE <>) OF unsigned(7 DOWNTO 0);
  --SIGNAL cart : arr_cart(0 TO 4095);
  --ATTRIBUTE ramstyle : string;
  --ATTRIBUTE ramstyle OF cart : SIGNAL IS "no_rw_check";
  
  SHARED VARIABLE cart : arr_cart(0 TO 16383) :=(OTHERS =>x"00");
  ATTRIBUTE ramstyle : string;
  ATTRIBUTE ramstyle OF cart : VARIABLE IS "no_rw_check";
  
  SIGNAL wcart : std_logic;
  
  SIGNAL vid_argb ,in_vid_argb ,ac_vid_argb : unsigned(3 DOWNTO 0);
  SIGNAL vid_de  ,in_vid_de  ,ac_vid_de   : std_logic;
  SIGNAL vid_hsyn,in_vid_hsyn,ac_vid_hsyn : std_logic;
  SIGNAL vid_vsyn,in_vid_vsyn,ac_vid_vsyn : std_logic;
  SIGNAL vid_ce  ,in_vid_ce  ,ac_vid_ce   : std_logic;

  SIGNAL in_vrst,ac_vrst : std_logic;
  -- OVO -----------------------------------------
  FUNCTION CC(i : character) RETURN unsigned IS
  BEGIN
    CASE i IS
      WHEN '0' => RETURN "00000";
      WHEN '1' => RETURN "00001";
      WHEN '2' => RETURN "00010";
      WHEN '3' => RETURN "00011";
      WHEN '4' => RETURN "00100";
      WHEN '5' => RETURN "00101";
      WHEN '6' => RETURN "00110";
      WHEN '7' => RETURN "00111";
      WHEN '8' => RETURN "01000";
      WHEN '9' => RETURN "01001";
      WHEN 'A' => RETURN "01010";
      WHEN 'B' => RETURN "01011";
      WHEN 'C' => RETURN "01100";
      WHEN 'D' => RETURN "01101";
      WHEN 'E' => RETURN "01110";
      WHEN 'F' => RETURN "01111";
      WHEN ' ' => RETURN "10000";
      WHEN '=' => RETURN "10001";
      WHEN '+' => RETURN "10010";
      WHEN '-' => RETURN "10011";
      WHEN '<' => RETURN "10100";
      WHEN '>' => RETURN "10101";
      WHEN '^' => RETURN "10110";
      WHEN 'v' => RETURN "10111";
      WHEN '(' => RETURN "11000";
      WHEN ')' => RETURN "11001";
      WHEN ':' => RETURN "11010";
      WHEN '.' => RETURN "11011";
      WHEN ',' => RETURN "11100";
      WHEN '?' => RETURN "11101";
      WHEN '|' => RETURN "11110";
      WHEN '#' => RETURN "11111";
      WHEN OTHERS => RETURN "10000";
    END CASE;
  END FUNCTION CC;
  FUNCTION CS(s : string) RETURN unsigned IS
    VARIABLE r : unsigned(0 TO s'length*5-1);
    VARIABLE j : natural :=0;
  BEGIN
    FOR i IN s'RANGE LOOP
      r(j TO j+4) :=CC(s(i));
      j:=j+5;
    END LOOP;
    RETURN r;
  END FUNCTION CS;
  
  SIGNAL vga_r_i,vga_r_u   : unsigned(7 DOWNTO 0);
  SIGNAL vga_g_i,vga_g_u   : unsigned(7 DOWNTO 0);
  SIGNAL vga_b_i,vga_b_u   : unsigned(7 DOWNTO 0);
  SIGNAL vga_de_i   : std_logic;
  
  SIGNAL ovo_ena  : std_logic;
  SIGNAL ovo_in0  : unsigned(0 TO 32*5-1) :=(OTHERS =>'0');
  SIGNAL ovo_in1  : unsigned(0 TO 32*5-1) :=(OTHERS =>'0');
  
  -- OVO -----------------------------------------
  FILE fil : text OPEN write_mode IS "trace_mem.log";
  
BEGIN

  hps : hps_io
    GENERIC MAP (
      STRLEN => CONF_STR'length)
    PORT MAP (
      clk_sys            => clksys,
      hps_bus            => hps_bus,
      conf_str           => to_slv(CONF_STR),
      buttons            => buttons,
      forced_scandoubler => OPEN,
      joystick_0         => joystick_0,
      joystick_1         => joystick_1,
      joystick_analog_0  => joystick_analog_0,
      joystick_analog_1  => joystick_analog_1,
      status             => status,
      sd_lba             => std_logic_vector'(x"0000_0000"),
      sd_rd              => '0',
      sd_wr              => '0',
      sd_ack             => OPEN,
      sd_conf            => '0',
      sd_ack_conf        => OPEN,
      sd_buff_addr       => OPEN,
      sd_buff_dout       => OPEN,
      sd_buff_din        => std_logic_vector'(x"00"),
      sd_buff_wr         => OPEN,
      img_mounted        => OPEN,
      img_size           => OPEN,
      img_readonly       => OPEN,
      ioctl_download     => ioctl_download,
      ioctl_index        => ioctl_index,
      ioctl_wr           => ioctl_wr,
      ioctl_addr         => ioctl_addr,
      ioctl_dout         => ioctl_dout,
      ioctl_wait         => ioctl_wait,
      rtc                => OPEN,
      timestamp          => OPEN,
      ps2_kbd_clk_out    => OPEN,
      ps2_kbd_data_out   => OPEN,
      ps2_kbd_clk_in     => '1',
      ps2_kbd_data_in    => '1',
      ps2_kbd_led_use    => std_logic_vector'("000"),
      ps2_kbd_led_status => std_logic_vector'("000"),
      ps2_mouse_clk_out  => OPEN,
      ps2_mouse_data_out => OPEN,
      ps2_mouse_clk_in   => '1',
      ps2_mouse_data_in  => '1',
      ps2_key            => ps2_key,
      ps2_mouse          => OPEN);

  ntsc_pal<=status(0);
  arca<=status(1); -- 0=Interton VC2000  1= Emerson Arcadia
  swap<=status(3);
  swapxy<=status(4); 

  ----------------------------------------------------------
  ipll : pll
    PORT MAP (
      refclk   => clk_50m,
      rst      => '0',
      outclk_0 => clksys_ntsc,
      outclk_1 => clksys_pal,
      locked   => pll_locked
      );
  
  clksys<=clksys_ntsc; --pal;
  
  -- CLK = Pix CLK * 8
  -- NTSC : 3.579545MHz   * 8
  -- PAL  : 4.43361875MHz * 8
  
  ----------------------------------------------------------
  -- Interton VC4000 & clones
  --  xx0 0aaa aaaa aaaa : Cardtrige : 2ko
  --  xx0 1aaa aaaa aaaa : RAM hobby computer / Cartdridge 4ko
  --  xx1 00aa aaaa aaaa : RAM option 1ko
  --  xx1 x110 1aaa aaaa : Key inputs
  --  xx1 x111 aaaa aaaa : Video PVI
  
  -- PVI : Programmable Video Interface
  i_sgs2636: ENTITY work.sgs2636
    PORT MAP (
      ad        => ad,
      dw        => dw,
      dr        => dr_pvi,
      req       => req_pvi,
      ack       => ack_pvi,
      wr        => wr,
      int       => int_pvi,
      intack    => intack,
      ivec      => ivec,
      vrst      => in_vrst,
      vid_argb  => in_vid_argb,
      vid_de    => in_vid_de,
      vid_hsyn  => in_vid_hsyn,
      vid_vsyn  => in_vid_vsyn,
      vid_ce    => in_vid_ce,
      sound     => in_sound,
      pot1      => pot1,
      pot2      => pot2,
      np        => ntsc_pal,
      reset     => reset,
      clk       => clksys,
      reset_na  => reset_na);

  dr_in_key<=in_volnoise  WHEN ad_delay(3 DOWNTO 0)=x"0" ELSE -- 1E80
          in_keypad1_1 WHEN ad_delay(3 DOWNTO 0)=x"8" ELSE -- 1E88
          in_keypad1_2 WHEN ad_delay(3 DOWNTO 0)=x"9" ELSE -- 1E89
          in_keypad1_3 WHEN ad_delay(3 DOWNTO 0)=x"A" ELSE -- 1E8A
          in_keypanel  WHEN ad_delay(3 DOWNTO 0)=x"B" ELSE -- 1E8B
          in_keypad2_1 WHEN ad_delay(3 DOWNTO 0)=x"C" ELSE -- 1E8C
          in_keypad2_2 WHEN ad_delay(3 DOWNTO 0)=x"D" ELSE -- 1E8D
          in_keypad2_3 WHEN ad_delay(3 DOWNTO 0)=x"E" ELSE -- 1E8E
          x"00";

  in_keypad1_1<=((key_rc & key_bp  & key_pc  & key_minus) OR
                 (joystick_0(11) & joystick_0(8)  & joystick_0(3) & joystick_0(0))) & "0000";
  in_keypad1_2<=((key_wc & key_reg & key_mem & key_plus) OR
                 (joystick_0(12) & joystick_0(9)  & joystick_0(6) & joystick_0(1))) & "0000";
  in_keypad1_3<=((key_c  & key_8   & key_4   & key_0   ) OR
                 (joystick_0(13) & joystick_0(10) & joystick_0(7) & joystick_0(2))) & "0000";
  
  in_keypad2_1<=((key_d  & key_9   & key_5   & key_1   ) OR
                 (joystick_1(11) & joystick_1(8)  & joystick_1(3) & joystick_1(0))) & "0000";
  in_keypad2_2<=((key_e  & key_a   & key_6   & key_2   ) OR
                 (joystick_1(12) & joystick_1(9)  & joystick_1(6) & joystick_1(1))) & "0000";
  in_keypad2_3<=((key_f  & key_b   & key_7   & key_3   ) OR
                 (joystick_1(13) & joystick_1(10) & joystick_1(7) & joystick_1(2))) & "0000";
  
  in_keypanel <=((key_select & key_start) OR
                 (joystick_0(4) & joystick_0(5)) OR
                 (joystick_1(4) & joystick_1(5))) & "000000";

  in_volnoise <=vol & icol & explo & noise & "000";
  
  -- Layout, most games :
  
  -- Key Layout, hobby computer :
  --   LEFT     RIGHT
  -- -  +  0      1  2  3
  -- PC Ad 4      5  6  7
  -- BP Rx 8      9  A  b
  -- R  W  C      d  E  F

  -- PS2 mappings
  ---  +  0      1  2  3
  --P  M  4      5  6  7
  --X  G  8      9  A  B
  --R  W  C      D  E  F

  -- Joystick mapping :
  -- 0  1  2   select=4
  -- 3  6  7   start =5
  -- 8  9 10
  --11 12 13     
  
  -- KEY    Mapped on keyboard :
  
  -- Additional buttons :
  -- START
  -- SELECT
  
  -- flag : Joystick : 0=Horizontal 1=Vertical
  pot2<=potr_v WHEN flag='1' ELSE potr_h;
  pot1<=potl_v WHEN flag='1' ELSE potl_h;
  
  ----------------------------------------------------------
  -- Emerson Arcadia & clones
  --  x00 aaaa aaaa aaaa : Cardtrige 4kb
  --  x01 1000 aaaa aaaa : Video UVI RAM  : 1800
  --  x01 1001 0xxx aaaa : Key inputs     : 1900
  --  x01 1001 1xxx xxxx : Video UVI regs : 1980
  --  x01 1010 aaaa aaaa : Video UVI RAM  : 1A00
  --  x10 aaaa aaaa aaaa : Cardridge high : 2000

  i_sgs2637: ENTITY work.sgs2637
    PORT MAP (
      ad        => ad,
      dw        => dw,
      dr        => dr_uvi,
      req       => req_uvi,
      ack       => ack_uvi,
      wr        => wr,
      vid_argb  => ac_vid_argb,
      vid_de    => ac_vid_de,
      vid_hsyn  => ac_vid_hsyn,
      vid_vsyn  => ac_vid_vsyn,
      vid_ce    => ac_vid_ce,
      vrst      => ac_vrst,
      sound     => ac_sound,
      pot1      => potr_v,
      pot2      => potl_v,
      pot3      => potr_h,
      pot4      => potl_h,
      np        => ntsc_pal,
      reset     => reset,
      clk       => clksys,
      reset_na  => reset_na);
  
  --   1 2 3
  --   4 5 6
  --   7 8 9
  -- ENT 0 CLR

  ac_keypad1_1<="0000" & ((key_1 & key_4 & key_7 & key_b) OR
                          (joystick_0(0) & joystick_0(3) & joystick_0(8) & joystick_0(11))) ; -- 1900
  ac_keypad1_2<="0000" & ((key_2 & key_5 & key_8 & key_0) OR
                          (joystick_0(1) & joystick_0(6) & joystick_0(9) & joystick_0(12))) ; -- 1901
  ac_keypad1_3<="0000" & ((key_3 & key_6 & key_9 & key_a) OR
                          (joystick_0(2) & joystick_0(7) & joystick_0(10) & joystick_0(13))) ; -- 1902
  
  ac_keypad2_1<="0000" & ((key_1 & key_4 & key_7 & key_b) OR
                          (joystick_1(0) & joystick_1(3) & joystick_1(8) & joystick_1(11))) ; -- 1904
  ac_keypad2_2<="0000" & ((key_2 & key_5 & key_8 & key_0) OR
                          (joystick_1(1) & joystick_1(6) & joystick_1(9) & joystick_1(12))) ; -- 1905
  ac_keypad2_3<="0000" & ((key_3 & key_6 & key_9 & key_a) OR
                          (joystick_1(2) & joystick_1(7) & joystick_1(10) & joystick_1(13))) ; -- 1906
  
  ac_keypanel <="00000" & ((key_reg & key_select & key_start) OR
                           (joystick_0(14) & joystick_0(4) & joystick_0(5)) OR
                           (joystick_1(14) & joystick_1(4) & joystick_1(5))); -- 1908. <Latched>
  
  dr_ac_key<=ac_keypad1_1 WHEN ad_delay(3 DOWNTO 0)=x"0" ELSE -- 1900
             ac_keypad1_2 WHEN ad_delay(3 DOWNTO 0)=x"1" ELSE -- 1901
             ac_keypad1_3 WHEN ad_delay(3 DOWNTO 0)=x"2" ELSE -- 1902
             ac_keypad2_1 WHEN ad_delay(3 DOWNTO 0)=x"4" ELSE -- 1904
             ac_keypad2_2 WHEN ad_delay(3 DOWNTO 0)=x"5" ELSE -- 1905
             ac_keypad2_3 WHEN ad_delay(3 DOWNTO 0)=x"6" ELSE -- 1906
             ac_keypanel  WHEN ad_delay(3 DOWNTO 0)=x"8" ELSE -- 1908
             x"00";
  
  ----------------------------------------------------------
  sense <=in_vrst WHEN arca='0' ELSE ac_vrst;
  
  potl_a<=mux(swap,unsigned(joystick_analog_1(15 DOWNTO 8)),
                   unsigned(joystick_analog_0(15 DOWNTO 8)))+x"80";
  potl_b<=mux(swap,unsigned(joystick_analog_1( 7 DOWNTO 0)),
                   unsigned(joystick_analog_0( 7 DOWNTO 0)))+x"80";
  potl_h<=mux(swapxy,potl_a,potl_b);
  potl_v<=mux(swapxy,potl_b,potl_a);
  
  potr_a<=mux(swap,unsigned(joystick_analog_0(15 DOWNTO 8)),
                   unsigned(joystick_analog_1(15 DOWNTO 8)))+x"80";
  potr_b<=mux(swap,unsigned(joystick_analog_0( 7 DOWNTO 0)),
                   unsigned(joystick_analog_1( 7 DOWNTO 0)))+x"80";
  potr_h<=mux(swapxy,potr_a,potr_b);
  potr_v<=mux(swapxy,potr_b,potr_a);
  
  ----------------------------------------------------------
  KeyCodes:PROCESS (clksys,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
         key_0<='0';
         key_1<='0';
         key_2<='0';
         key_3<='0';
         key_4<='0';
         key_5<='0';
         key_6<='0';
         key_7<='0';
         key_8<='0';
         key_9<='0';
         key_a<='0';
         key_b<='0';
         key_c<='0';
         key_d<='0';
         key_e<='0';
         key_f<='0';
         key_rc<='0'; -- R
         key_wc<='0'; -- W
         key_reg <= '0'; -- G
         key_pc  <= '0'; -- P
         key_mem <= '0'; -- M
         key_bp  <= '0'; -- X
         key_plus <='0'; -- +
         key_minus<='0'; -- -
         key_select<='0'; -- SPACE
         key_start <='0'; -- RETURN
    ELSIF rising_edge(clksys) THEN
      ps2_key_delay<=ps2_key;
      IF ps2_key_delay(10)/=ps2_key(10) THEN
        CASE ps2_key(7 DOWNTO 0) IS
          WHEN x"45" => key_0<=ps2_key(9);
          WHEN x"16" => key_1<=ps2_key(9);
          WHEN x"1E" => key_2<=ps2_key(9);
          WHEN x"26" => key_3<=ps2_key(9);
          WHEN x"25" => key_4<=ps2_key(9);
          WHEN x"2E" => key_5<=ps2_key(9);
          WHEN x"36" => key_6<=ps2_key(9);
          WHEN x"3D" => key_7<=ps2_key(9);
          WHEN x"3E" => key_8<=ps2_key(9);
          WHEN x"46" => key_9<=ps2_key(9);
          WHEN x"1C" => key_a<=ps2_key(9);
          WHEN x"32" => key_b<=ps2_key(9);
          WHEN x"21" => key_c<=ps2_key(9);
          WHEN x"23" => key_d<=ps2_key(9);
          WHEN x"24" => key_e<=ps2_key(9);
          WHEN x"2B" => key_f<=ps2_key(9);
          WHEN x"2D" => key_rc<=ps2_key(9); -- R
          WHEN x"1D" => key_wc<=ps2_key(9); -- W
          WHEN X"35" => key_reg <= ps2_key(9); -- G
          WHEN X"4D" => key_pc  <= ps2_key(9); -- P
          WHEN X"3A" => key_mem <= ps2_key(9); -- M
          WHEN x"22" => key_bp  <= ps2_key(9); -- X
          WHEN x"09" => key_plus <=ps2_key(9); -- +
          WHEN x"4E" => key_minus<=ps2_key(9); -- -
          WHEN x"29" => key_select<=ps2_key(9); -- SPACE
          WHEN x"5A" => key_start <=ps2_key(9); -- RETURN
          WHEN OTHERS => NULL;
        END CASE;
      END IF;
    END IF;
  END PROCESS KeyCodes;

  --key_start<='0',
  --            '1' AFTER 200 ms,
  --            '0' AFTER 500 ms;
  
  ----------------------------------------------------------
  
  dr<=dr_pvi    WHEN arca='0' AND
        ad_delay(12)='1' AND ad_delay(11 DOWNTO 8)="1111" ELSE -- PVI Interton
      dr_in_key WHEN arca='0' AND
        ad_delay(12)='1' AND ad_delay(11 DOWNTO 8)="1110" ELSE
        
      dr_uvi    WHEN arca='1' AND
        ad_delay(12)='1' AND ad_delay(11 DOWNTO 8)="1000" ELSE -- UVI Arcadia
      dr_uvi    WHEN arca='1' AND
        ad_delay(12)='1' AND ad_delay(11 DOWNTO 7)="10011" ELSE -- UVI Arcadia
      dr_ac_key WHEN arca='1' AND
        ad_delay(12)='1' AND ad_delay(11 DOWNTO 7)="10010" ELSE -- Keyboard
      dr_uvi    WHEN arca='1' AND
        ad_delay(12)='1' AND ad_delay(11 DOWNTO 8)="1010" ELSE -- UVI Arcadia
        
      dr_rom  -- Cardridge
      ;
  
  req_pvi<=NOT arca AND req AND tick_cpu WHEN
            ad(12)='1' AND ad(10 DOWNTO 8)="111" ELSE '0';
  
  req_uvi<=arca AND req AND tick_cpu WHEN
            (ad(12)='1' AND ad(11 DOWNTO 8)="1000") OR
            (ad(12)='1' AND ad(11 DOWNTO 8)="1010") OR
            (ad(12)='1' AND ad(11 DOWNTO 7)="10011") ELSE '0';

  ad_rom <="000" & ad(11 DOWNTO 0) WHEN arca='1' AND ad(14 DOWNTO 12)="000" ELSE
           "001" & ad(11 DOWNTO 0) WHEN arca='1' AND ad(14 DOWNTO 12)="010" ELSE

            ad;
  -- CPU
  i_sgs2650: ENTITY work.sgs2650
    PORT MAP (
      req      => req,
      ack      => ack,
      ad       => ad,
      wr       => wr,
      dw       => dw,
      dr       => dr,
      mio      => mio,
      ene      => ene,
      dc       => dc,
      ph       => ph,
      int      => int,
      intack   => intack,
      ivec     => ivec,
      sense    => sense,
      flag     => flag,
      reset    => creset,
      clk      => clksys,
      reset_na => reset_na);
  
  ----------------------------------------------------------
--pragma synthesis_off
  Dump:PROCESS IS
    VARIABLE lout : line;
    VARIABLE doread : boolean := false;
    VARIABLE adr : uv15;
  BEGIN
    wure(clksys);
    IF doread THEN
      write(lout,"RD(" & to_hstring('0' & adr) & ")=" & to_hstring(dr));
      writeline(fil,lout);
      doread:=false;
    END IF;
    IF req='1' AND ack='1' AND reset='0' AND reset_na='1' THEN
      IF wr='1' THEN
        write(lout,"WR(" & to_hstring('0' & ad) & ")=" & to_hstring(dw));
        writeline(fil,lout);
      ELSE
        doread:=true;
        adr:=ad;
      END IF;
    END IF;
  END PROCESS Dump;

--pragma synthesis_on
  ----------------------------------------------------------
  
  ad_delay<=ad WHEN rising_edge(clksys);
  
  vga_r_i<=(7=>vid_argb(2) AND vid_argb(3),OTHERS => vid_argb(2));
  vga_g_i<=(7=>vid_argb(1) AND vid_argb(3),OTHERS => vid_argb(1));
  vga_b_i<=(7=>vid_argb(0) AND vid_argb(3),OTHERS => vid_argb(0));
  
  vga_sl<="00";
  vga_f1<='0';
  
  clk_video<=clksys;
  ce_pixel<=vid_ce;
  
  audio_l<=std_logic_vector(sound) & x"00";
  audio_r<=std_logic_vector(sound) & x"00";
  audio_s<='1';
  audio_mix<="11";

  int<=int_pvi AND NOT arca;
  
  ----------------------------------------------------------
  -- MUX VIDEO
  
  vid_argb<=mux(arca,ac_vid_argb,in_vid_argb)  WHEN rising_edge(clksys);
  vid_de  <=mux(arca,ac_vid_de  ,in_vid_de )  WHEN rising_edge(clksys);
  vid_hsyn<=mux(arca,ac_vid_hsyn,in_vid_hsyn) WHEN rising_edge(clksys);
  vid_vsyn<=mux(arca,ac_vid_vsyn,in_vid_vsyn) WHEN rising_edge(clksys);
  vid_ce  <=mux(arca,ac_vid_ce  ,in_vid_ce)   WHEN rising_edge(clksys);
  video_arx <= X"04";
  video_ary <= X"03";

  sound <=mux(arca,ac_sound,in_sound) WHEN rising_edge(clksys);
  ----------------------------------------------------------
  i_ovo: ENTITY work.ovo
    PORT MAP (
      i_r     => vga_r_i,
      i_g     => vga_g_i,
      i_b     => vga_b_i,
      i_hs    => vid_hsyn,
      i_vs    => vid_vsyn,
      i_de    => vid_de,
      i_en    => vid_ce,
      i_clk   => clksys,
      o_r     => vga_r_u,
      o_g     => vga_g_u,
      o_b     => vga_b_u,
      o_hs    => vga_hs,
      o_vs    => vga_vs,
      o_de    => vga_de,
      ena     => ovo_ena,
      in0     => ovo_in0,
      in1     => ovo_in1);

  vga_r<=std_logic_vector(vga_r_u);
  vga_g<=std_logic_vector(vga_g_u);
  vga_b<=std_logic_vector(vga_b_u);
  
  ovo_in0<=
    "00" & ad(14 DOWNTO 12) &
    '0'  & ad(11 DOWNTO 8) &
    '0'  & ad( 7 DOWNTO 4) &
    '0'  & ad( 3 DOWNTO 0) &
    CC(' ') &
    '0' & unsigned(joystick_0(15 DOWNTO 12)) &
    '0' & unsigned(joystick_0(11 DOWNTO 8)) &
    '0' & unsigned(joystick_0(7 DOWNTO 4)) &
    '0' & unsigned(joystick_0(3 DOWNTO 0)) &
    CC(' ') &
    '0' & potl_v(7 DOWNTO 4) &
    '0' & potl_v(3 DOWNTO 0) &
    CC(' ') &
    '0' & potl_h(7 DOWNTO 4) &
    '0' & potl_h(3 DOWNTO 0) &
    CC(' ') &
    "0000" & ps2_key(10) &
    "0000" & ps2_key(9) &
    "0000" & ps2_key(8) &
    '0' & unsigned(ps2_key(7 DOWNTO 4)) &
    '0' & unsigned(ps2_key(3 DOWNTO 0)) &
    CC(' ') &
    '0' & in_keypanel(7 DOWNTO 4) &
    CC(' ') &
    '0' & in_keypad1_1(7 DOWNTO 4) &
    '0' & in_keypad1_2(7 DOWNTO 4) &
    '0' & in_keypad1_3(7 DOWNTO 4) &
    '0' & in_keypad2_1(7 DOWNTO 4) &
    '0' & in_keypad2_2(7 DOWNTO 4) &
    '0' & in_keypad2_3(7 DOWNTO 4) &
    CC('.') &
    CC(' ');
    
  ovo_in1<=ovo_in0;
  
  ovo_ena<=status(2); -- Overlay
  led_power<='1' & vid_vsyn;
  led_disk <='1' & vid_hsyn;
  
  ----------------------------------------------------------
  -- ROM / RAM

  wcart<=wr AND req AND ack; -- WHEN ad(12)='0' ELSE '0';
  
  icart:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      dr_rom<=cart(to_integer(ad_rom(13 DOWNTO 0))); -- 8kB
      
      IF wcart='1' THEN
        -- RAM
        cart(to_integer(ad_rom(13 DOWNTO 0))):=dw;
      END IF;
    END IF;
  END PROCESS icart;

  icart2:PROCESS(clksys) IS
  BEGIN
    IF rising_edge(clksys) THEN
      -- Download
      IF w_wr='1' THEN
        cart(to_integer(w_a)):=w_d;
      END IF;
    END IF;
  END PROCESS icart2;
  
  PROCESS(clksys,reset_na) IS
  BEGIN
    IF rising_edge(clksys) THEN
      w_wr<=ioctl_download AND ioctl_wr;
      w_d <=unsigned(ioctl_dout);
      w_a <=unsigned(ioctl_addr(12 DOWNTO 0));
    END IF;
  END PROCESS;
  
  ioctl_wait<='0';

  ----------------------------------------------------------
  -- CPU CLK
  DivCLK:PROCESS (clksys,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
      tick_cpu<='0';
    ELSIF rising_edge(clksys) THEN
      IF tick_cpu_cpt=CDIV - 1 THEN
        tick_cpu_cpt<=0;
        tick_cpu<='1';
      ELSE
        tick_cpu_cpt<=tick_cpu_cpt+1;
        tick_cpu<='0';
      END IF;
    END IF;
  END PROCESS DivCLK;
  
  ack<=tick_cpu;
  
  reset_na<=NOT reset OR pll_locked;
  creset<=ioctl_download;
  
END struct;
