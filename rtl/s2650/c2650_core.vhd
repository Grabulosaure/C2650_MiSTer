---------------------------------------------------------------------------------
-- Games consoles with Signetics 2650 CPU

-- Video 2636 PVI. Interton VC4000 & clones
-- Video 2637 UVI. Emerson Arcadia & clones

---------------------------------------------------------------------------------

LIBRARY IEEE;
USE IEEE.std_logic_1164.all;
USE IEEE.numeric_std.all;

USE std.textio.ALL;

LIBRARY work;
USE work.base_pack.ALL;

ENTITY c2650_core IS
  PORT (
    -- Master input clock
    clk              : IN    std_logic;
    
    -- Async reset from top-level module. Can be used as initial reset.
    reset            : IN    std_logic;

    -- Must be passed to hps_io module
    ntsc_pal         : IN    std_logic;
    arca             : IN    std_logic;
    swap             : IN    std_logic;
    swapxy           : IN    std_logic;
    bright           : IN    std_logic;

    -- Base video clock. Usually equals to CLK_SYS.
    clk_video        : OUT   std_logic;

    -- Multiple resolutions are supported using different CE_PIXEL rates.
    -- Must be based on CLK_VIDEO
    ce_pixel         : OUT   std_logic;

    -- VGA
    vga_r            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_g            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_b            : OUT   std_logic_vector(7 DOWNTO 0);
    vga_hs           : OUT   std_logic; -- positive pulse!
    vga_vs           : OUT   std_logic; -- positive pulse!
    vga_de           : OUT   std_logic; -- = not (VBlank or HBlank)

    -- AUDIO
    sound            : OUT   std_logic_vector(7 DOWNTO 0);
    
    ps2_key           : IN  std_logic_vector(10 DOWNTO 0);    
    joystick_0        : IN  std_logic_vector(31 DOWNTO 0);
    joystick_1        : IN  std_logic_vector(31 DOWNTO 0);
    joystick_analog_0 : IN  std_logic_vector(15 DOWNTO 0);
    joystick_analog_1 : IN  std_logic_vector(15 DOWNTO 0);
    
    ioctl_download    : IN  std_logic;
    ioctl_index       : IN  std_logic_vector(7 DOWNTO 0);
    ioctl_wr          : IN  std_logic;
    ioctl_addr        : IN  std_logic_vector(24 DOWNTO 0);
    ioctl_dout        : IN  std_logic_vector(7 DOWNTO 0);
    ioctl_wait        : OUT std_logic
    );
END c2650_core;

ARCHITECTURE struct OF c2650_core IS

  CONSTANT CDIV : natural := 4 * 8;
 
  SIGNAL ps2_key_delay : std_logic_vector(10 DOWNTO 0);
  
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
  SIGNAL in_vol : unsigned(1 DOWNTO 0);
  SIGNAL in_icol,in_explo,in_explo2,in_noise,in_snd : std_logic;
  SIGNAL in_sound,ac_sound,in_sound2 : unsigned(7 DOWNTO 0);
  SIGNAL lfsr : uv15;
  SIGNAL explo : natural RANGE 0 TO 1000000;
  SIGNAL divlfsr : uint8;
  
  SIGNAL pot1,pot2 : unsigned(7 DOWNTO 0);
  SIGNAL potl_a,potl_b,potr_a,potr_b : unsigned(7 DOWNTO 0);
  SIGNAL potl_v,potl_h,potr_v,potr_h : unsigned(7 DOWNTO 0);
  
  SIGNAL tick_cpu_cpt : natural RANGE 0 TO CDIV-1;
  SIGNAL tick_cpu : std_logic;
  
  SIGNAL ad,ad_delay,ad_rom : unsigned(14 DOWNTO 0);
  SIGNAL dr,dw,dr_pvi,dr_uvi,dr_rom,dr_in_key,dr_ac_key : unsigned(7 DOWNTO 0);
  SIGNAL req,req_pvi,req_uvi,req_mem : std_logic;
  SIGNAL ack,ackp,ack_pvi,ack_uvi,ack_mem : std_logic;
  SIGNAL sel_pvi,sel_uvi,sel_mem : std_logic;
  SIGNAL ack_mem_p,ack_mem_p2 : std_logic :='0';
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
  
  SIGNAL vga_argb ,in_vga_argb ,ac_vga_argb : unsigned(3 DOWNTO 0);
  SIGNAL in_vga_de  ,ac_vga_de   : std_logic;
  SIGNAL in_vga_hsyn,ac_vga_hsyn : std_logic;
  SIGNAL in_vga_vsyn,ac_vga_vsyn : std_logic;
  SIGNAL in_vga_ce  ,ac_vga_ce   : std_logic;
  
  SIGNAL in_vrst,ac_vrst : std_logic;
  
  SIGNAL vga_r_i,vga_g_i,vga_b_i : uv8;
  SIGNAL vga_de_i : std_logic;
  
  FILE fil : text OPEN write_mode IS "trace_mem.log";
  
BEGIN

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
      tick      => tick_cpu,
      int       => int_pvi,
      intack    => intack,
      ivec      => ivec,
      vrst      => in_vrst,
      vid_argb  => in_vga_argb,
      vid_de    => in_vga_de,
      vid_hsyn  => in_vga_hsyn,
      vid_vsyn  => in_vga_vsyn,
      vid_ce    => in_vga_ce,
      sound     => in_sound,
      icol      => in_icol,
      bright    => bright,
      pot1      => pot1,
      pot2      => pot2,
      np        => ntsc_pal,
      reset     => reset,
      clk       => clk,
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
                 (joystick_1(11) & joystick_1(8)  & joystick_1(3) & joystick_1(07))) & "0000";
  in_keypad2_2<=((key_e  & key_a   & key_6   & key_2   ) OR
                 (joystick_1(12) & joystick_1(9)  & joystick_1(6) & joystick_1(1))) & "0000";
  in_keypad2_3<=((key_f  & key_b   & key_7   & key_3   ) OR
                 (joystick_1(13) & joystick_1(10) & joystick_1(7) & joystick_1(2))) & "0000";
  
  in_keypanel <=((key_select & key_start) OR
                 (joystick_0(4) & joystick_0(5)) OR
                 (joystick_1(4) & joystick_1(5))) & "000000";

  in_volnoise <=in_vol & in_icol & in_explo & in_noise & in_snd & "00";

  PROCESS(clk,reset_na) IS
    VARIABLE a_v,b_v : uv8;
  BEGIN
    IF reset_na='0' THEN
      in_vol<="00";
      in_icol<='0';
      in_explo<='0';
      in_noise<='0';
      in_snd<='0';
      lfsr<=to_unsigned(1,15);
      
    ELSIF rising_edge(clk) THEN
      IF arca='0' AND ad_delay(3 DOWNTO 0)=x"0" AND ad_delay(12)='1' AND
        ad_delay(11 DOWNTO 8)="1110" AND wr='1' AND tick_cpu='1' THEN
        in_vol<=dw(7 DOWNTO 6);
        in_icol<=dw(5);
        in_explo<=dw(4);
        in_noise<=dw(3);
        in_snd<=dw(2);
      END IF;
      in_explo2<=in_explo;
      
      --------------------------------
      IF in_explo='1' AND in_explo2='0' THEN
        explo<=100000;
      END IF;
      IF tick_cpu='1' AND explo/=0 THEN
        explo<=explo-1;
      END IF;
      
      --------------------------------
      IF tick_cpu='1' THEN
        divlfsr<=(divlfsr+1) MOD 64;
        IF divlfsr=0 THEN
          lfsr<=('0' & lfsr(14 DOWNTO 1)) XOR
                 (lfsr(0) & "0000000000000" & lfsr(0));
        END IF;
      END IF;
      
      CASE in_vol IS
        WHEN "00"   => a_v:=mux(in_sound(7),x"C0",x"3F");
        WHEN "01"   => a_v:=mux(in_sound(7),x"E0",x"1F");
        WHEN "10"   => a_v:=mux(in_sound(7),x"F0",x"0F");
        WHEN OTHERS => a_v:=mux(in_sound(7),x"F8",x"07");
      END CASE;
      IF in_snd='0' THEN
        a_v:=x"00";
      END IF;
      b_v:=x"00";
      IF in_noise='1' THEN
        b_v:=mux(lfsr(0),x"08",x"f8");
      END IF;
      IF explo/=0 THEN
        b_v:=mux(lfsr(0),x"20",x"E0");
      END IF;
      in_sound2<=a_v + b_v;
      
    --------------------------------
    END IF;
  END PROCESS;
  
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
      tick      => tick_cpu,
      vid_argb  => ac_vga_argb,
      vid_de    => ac_vga_de,
      vid_hsyn  => ac_vga_hsyn,
      vid_vsyn  => ac_vga_vsyn,
      vid_ce    => ac_vga_ce,
      vrst      => ac_vrst,
      sound     => ac_sound,
      pot1      => potr_v,
      pot2      => potl_v,
      pot3      => potr_h,
      pot4      => potl_h,
      np        => ntsc_pal,
      reset     => reset,
      clk       => clk,
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
  KeyCodes:PROCESS (clk,reset_na) IS
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
    ELSIF rising_edge(clk) THEN
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
        ad_delay(12)='1' AND ad_delay(10 DOWNTO 8)="111" ELSE -- PVI Interton
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
  
  sel_pvi<=NOT arca WHEN
            ad(12)='1' AND ad(10 DOWNTO 8)="111" ELSE '0';
  
  sel_uvi<=arca WHEN
            (ad(12)='1' AND ad(11 DOWNTO 8)="1000") OR
            (ad(12)='1' AND ad(11 DOWNTO 8)="1010") OR
            (ad(12)='1' AND ad(11 DOWNTO 7)="10011") ELSE '0';

  sel_mem<=NOT sel_pvi AND NOT sel_uvi;
  
  req_pvi<=sel_pvi AND req;
  req_uvi<=sel_uvi AND req;
  req_mem<=sel_mem AND req;
  
  ackp<=tick_cpu AND ack_pvi WHEN sel_pvi='1' ELSE
        tick_cpu AND ack_uvi WHEN sel_uvi='1' ELSE
        tick_cpu AND ack_mem;

  PROCESS (clk) IS
  BEGIN
    IF rising_edge(clk) THEN
      IF tick_cpu='1' THEN
        ack_mem_p<=req_mem AND NOT ack_mem;
        ack_mem_p2<=ack_mem_p AND req_mem;
      END IF;
    END IF;
  END PROCESS;
  ack_mem<=ack_mem_p2 AND ack_mem_p;
  
  --ack<='0';

  ack<=ackp WHEN rising_edge(clk);
  
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
      clk      => clk,
      reset_na => reset_na);
  
  int<=int_pvi AND NOT arca;
  ad_delay<=ad WHEN rising_edge(clk);
  
  ----------------------------------------------------------
--pragma synthesis_off
  Dump:PROCESS IS
    VARIABLE lout : line;
    VARIABLE doread : boolean := false;
    VARIABLE adr : uv15;
  BEGIN
    wure(clk);
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
  
  sound <=std_logic_vector(mux(arca,ac_sound,in_sound2)) WHEN rising_edge(clk);
  
  ----------------------------------------------------------
  -- MUX VIDEO
  clk_video<=clk;
  ce_pixel<=mux(arca,ac_vga_ce,in_vga_ce) WHEN rising_edge(clk);
  
  vga_de<=mux(arca,ac_vga_de  ,in_vga_de )  WHEN rising_edge(clk);
  vga_hs<=mux(arca,ac_vga_hsyn,in_vga_hsyn) WHEN rising_edge(clk);
  vga_vs<=mux(arca,ac_vga_vsyn,in_vga_vsyn) WHEN rising_edge(clk);
  
  vga_argb<=mux(arca,ac_vga_argb,in_vga_argb)  WHEN rising_edge(clk);
  vga_r_i<=(7=>vga_argb(2) AND vga_argb(3),OTHERS => vga_argb(2));
  vga_g_i<=(7=>vga_argb(1) AND vga_argb(3),OTHERS => vga_argb(1));
  vga_b_i<=(7=>vga_argb(0) AND vga_argb(3),OTHERS => vga_argb(0));
  vga_r<=std_logic_vector(vga_r_i);
  vga_g<=std_logic_vector(vga_g_i);
  vga_b<=std_logic_vector(vga_b_i);
  
  ----------------------------------------------------------
  -- ROM / RAM

  wcart<=wr AND req AND ack; -- WHEN ad(12)='0' ELSE '0';
  
  icart:PROCESS(clk) IS
  BEGIN
    IF rising_edge(clk) THEN
      dr_rom<=cart(to_integer(ad_rom(13 DOWNTO 0))); -- 8kB
      
      IF wcart='1' THEN
        -- RAM
        cart(to_integer(ad_rom(13 DOWNTO 0))):=dw;
      END IF;
    END IF;
  END PROCESS icart;

  icart2:PROCESS(clk) IS
  BEGIN
    IF rising_edge(clk) THEN
      -- Download
      IF w_wr='1' THEN
        cart(to_integer(w_a)):=w_d;
      END IF;
    END IF;
  END PROCESS icart2;
  
  PROCESS(clk) IS
  BEGIN
    IF rising_edge(clk) THEN
      w_wr<=ioctl_download AND ioctl_wr;
      w_d <=unsigned(ioctl_dout);
      w_a <=unsigned(ioctl_addr(12 DOWNTO 0));
    END IF;
  END PROCESS;
  
  ioctl_wait<='0';
  
  ----------------------------------------------------------
  -- CPU CLK
  DivCLK:PROCESS (clk,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
      tick_cpu<='0';
    ELSIF rising_edge(clk) THEN
      IF tick_cpu_cpt=CDIV - 1 THEN
        tick_cpu_cpt<=0;
        tick_cpu<='1';
      ELSE
        tick_cpu_cpt<=tick_cpu_cpt+1;
        tick_cpu<='0';
      END IF;
    END IF;
  END PROCESS DivCLK;
  
  reset_na<=NOT reset OR NOT ioctl_download;
  creset<=ioctl_download;
  
END struct;
