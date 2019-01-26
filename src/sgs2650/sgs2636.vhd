--------------------------------------------------------------------------------
-- Signetics 2636 PVI Programmable Video Interface
--------------------------------------------------------------------------------
-- DO 5/2018
--------------------------------------------------------------------------------
-- 7xx
-- F00       : Object 1 : Shape 0
-- F01       : Object 1 : Shape 1
-- F02       : Object 1 : Shape 2
-- F03       : Object 1 : Shape 3
-- F04       : Object 1 : Shape 4
-- F05       : Object 1 : Shape 5
-- F06       : Object 1 : Shape 6
-- F07       : Object 1 : Shape 7
-- F08       : Object 1 : Shape 8
-- F09       : Object 1 : Shape 9
-- F0A       : Object 1 : HC  : Horizontal coordinate
-- F0B       : Object 1   HCB : Horizontal coordinate replicate 
-- F0C       : Object 1 : VC  : Vertical   coordinate
-- F0D       : Object 1 : VCB : Vectical   coordinate replicate
-- F0E - F0F : SCRATCH : 2 bytes
-- F10 - F1D : Object 2
-- F1E - F1F : SCRATCH : 2 bytes
-- F20 - F2D : Object 3
-- F2E - F3F : <undef>
-- F40 - F4D : Object 4
-- F4E - F6D : SCRATCH : 32 bytes
-- F6E - F7F : <undef>
-- F80 - FA7 : Background : Vertical bars
-- FA8 - FAC : Background : Horizontal bars
-- FAD       : SCRATCH : 1 byte
-- FAE - FBF : <undef>
-- FC0       : Object sizes
-- FC1       : Object 1 & 2 colours
-- FC2       : Object 3 & 4 colours
-- FC3       : Score format & position
-- FC4 - FC5 : <undef>
-- FC6       : Background colour
-- FC7       : Sound
-- FC8       : Score N1 & N2
-- FC9       : Score N3 & N4
-- FCA       : Collision (read)
-- FCB       : Collision (read)
-- FCC       : POT1
-- FCD       : POT2
-- FCE - FCF : <undef>
-- FD0 - FDF : Mirror FC0 - FCF
-- FE0 - FEF : Mirror FC0 - FCF
-- FFO - FFF : Mirror FC0 - FCF

----------------------------------------------------------------
-- NTSC 60Hz. 480i
-- F = 3.579545MHz = 315 / 88 MHz
-- H=227 clocks
-- V=525/2 lines
--                 21 lines vblank
--              60Hz
-- Line rate 15734 HZ= (3.579545×2/455 MHz = 9/572 MHz)
                                           
--483 lines images + synchro = 525 lines

-- Divide by 262

----------------------------------------------------------------
-- PAL 50Hz. 576i
-- F = 4.43MHz = 4.43361875MHz
-- H= 283.75  clocks

-- 576 lignes image + synchro = 625 lines

-- Divide by 312
----------------------------------------------------------------

-- Horizontal : 32 .. 227
-- Vertical   : 20 .. 252

-- Input clock shall be 8x the pixel clock
--  0 : Clear
--  1 : Vertical   background
--  2 : Horizontal background
--  3 : Object 1
--  4 : Object 2
--  5 : Object 3
--  6 : Object 4
--  7 : Score

----------------------------------------------------------------
-- Zone affichage : Blocs 8x20.   16 x 10 blocs = 128 x 200 pixels
-- Zone fond      : H= 32..159  V=20..219
-- Zone affichage : H=  0..     V= 0..251
-- Résolution : 227 x 253

-- Nombres : 12x20 PIX

-- 227 pulses @ 3.58MHz/line

--  0 SET 1
--  1 SET 1
--  2 SET 2 TOP
--  3 SET 2 TOP
--  4 SET 2 TOP
--  5 SET 2 TOP
--  6 SET 2 TOP
--  7 SET 2 TOP
--  8 SET 2 TOP
--  9 SET 2 TOP
-- 10 SET 2 TOP
-- 11 SET 2 BOTTOM
-- 12 SET 2 BOTTOM
-- 13 SET 2 BOTTOM
-- 14 SET 2 BOTTOM
-- 15 SET 2 BOTTOM
-- 16 SET 2 BOTTOM
-- 17 SET 2 BOTTOM
-- 18 SET 2 BOTTOM
-- 19 SET 2 BOTTOM

--------------------------------------------------------------------------------

LIBRARY ieee;
USE ieee.std_logic_1164.ALL;
USE ieee.numeric_std.ALL;

USE std.textio.ALL;

LIBRARY work;
USE work.base_pack.ALL;

ENTITY sgs2636 IS
  PORT (
    ad   : IN  uv15;      -- Address bus
    
    dw   : IN  uv8;       -- Data write
    dr   : OUT uv8;       -- Data read
    
    req  : IN  std_logic;
    ack  : OUT std_logic;
    wr   : IN  std_logic;
    
    int    : OUT std_logic;
    intack : IN  std_logic;
    ivec   : OUT uv8;

    vid_rgb   : OUT uv3;         -- R | G | B. 1bit/color
    vid_de    : OUT std_logic;
    vid_hsyn  : OUT std_logic;
    vid_vsyn  : OUT std_logic;
    vid_ce    : OUT std_logic;

    sound    : OUT uv8;

    pot1     : IN uv8;
    pot2     : IN uv8;

    np       : IN std_logic; -- 0=NTSC 60Hz, 1=PAL 50Hz
    
    reset    : IN std_logic;
    clk      : IN std_logic; -- 8x Pixel clock
    reset_na : IN std_logic
    );
END ENTITY sgs2636;

ARCHITECTURE rtl OF sgs2636 IS
  SUBTYPE uint9 IS natural RANGE 0 TO 511;

  SIGNAL wreq : std_logic;
  SIGNAL mem : arr_uv8(0 TO 255);
  ATTRIBUTE ramstyle : string;
  ATTRIBUTE ramstyle OF mem : SIGNAL IS "no_rw_check";

  SIGNAL bg_dr : uv8;
  SIGNAL mad,mdr,mdrp : uv8;
  SIGNAL drreg_sel : std_logic;
  SIGNAL dr_mem,dr_reg : uv8;
  
  SIGNAL osize,ocol12,ocol34,spos,bgcolour,sper : uv8 :=x"00";
  SIGNAL sval12,sval34,bgcoll,ocoll : uv8 :=x"00";
  SIGNAL clr_coll,clr_ocoll : std_logic;
  SIGNAL pre_coll,pre_ocoll : std_logic;
  SIGNAL o1_hit,o2_hit,o3_hit,o4_hit,bg_hit : std_logic;
  ALIAS bg_ena  : std_logic IS bgcolour(3); -- Background Enable
  ALIAS bg_colour : uv3 IS bgcolour(6 DOWNTO 4); -- Background colour
  ALIAS sc_colour : uv3 IS bgcolour(2 DOWNTO 0); -- Screen colour
  ALIAS o4_size : uv2 IS osize(7 DOWNTO 6); -- Object 4 size
  ALIAS o3_size : uv2 IS osize(5 DOWNTO 4); -- Object 3 size
  ALIAS o2_size : uv2 IS osize(3 DOWNTO 2); -- Object 2 size
  ALIAS o1_size : uv2 IS osize(1 DOWNTO 0); -- Object 1 size
  ALIAS o1_col  : uv3 IS ocol12(5 DOWNTO 3); -- Object 1 colour
  ALIAS o2_col  : uv3 IS ocol12(2 DOWNTO 0); -- Object 2 colour
  ALIAS o3_col  : uv3 IS ocol34(5 DOWNTO 3); -- Object 3 colour
  ALIAS o4_col  : uv3 IS ocol34(2 DOWNTO 0); -- Object 4 colour
  ALIAS s_pos   : std_logic IS spos(0); -- Score position
  ALIAS s_form  : std_logic IS spos(1); -- Score format
  ALIAS s1_val  : uv4 IS sval12(7 DOWNTO 4); -- Score value 1
  ALIAS s2_val  : uv4 IS sval12(3 DOWNTO 0); -- Score value 2
  ALIAS s3_val  : uv4 IS sval34(7 DOWNTO 4); -- Score value 3
  ALIAS s4_val  : uv4 IS sval34(3 DOWNTO 0); -- Score value 4

  SIGNAL o1_hc,o1_hcb,o1_hcbm,o1_vc,o1_vcm,o1_vcb,o1_vcbm : uv8 :=x"00";
  SIGNAL o2_hc,o2_hcb,o2_hcbm,o2_vc,o2_vcm,o2_vcb,o2_vcbm : uv8 :=x"00";
  SIGNAL o3_hc,o3_hcb,o3_hcbm,o3_vc,o3_vcm,o3_vcb,o3_vcbm : uv8 :=x"00";
  SIGNAL o4_hc,o4_hcb,o4_hcbm,o4_vc,o4_vcm,o4_vcb,o4_vcbm : uv8 :=x"00";

  SIGNAL o12_coll,o13_coll,o23_coll,o14_coll,o34_coll,o24_coll : std_logic;
  SIGNAL o1b_coll,o2b_coll,o3b_coll,o4b_coll : std_logic;
  SIGNAL o1_cplt,o2_cplt,o3_cplt,o4_cplt : std_logic;
  SIGNAL o1_cpltm,o2_cpltm,o3_cpltm,o4_cpltm : std_logic;
  
  SIGNAL o1_diff,o2_diff,o3_diff,o4_diff : uint9 :=0;
  SIGNAL o1_diff_clr,o2_diff_clr,o3_diff_clr,o4_diff_clr : std_logic;
  SIGNAL hpos,htotal,hsyncstart,hsyncend,hdisp : uint9;
  SIGNAL vpos,vpos1,vpos2,vtotal,vsyncstart,vsyncend,vdisp : uint9;
  SIGNAL hpix : uint9;
  SIGNAL vbar   : uint5; -- Vertical bar number 0 .. 19
  SIGNAL vpos20 : uint5; -- Line number within bar 0..19

  SIGNAL divi : natural RANGE 0 TO 8-1;
  SIGNAL tick_divi : std_logic;
  
  SIGNAL hpulse,sound_i,int_i : std_logic;
  SIGNAL snd_cpt : uv8;
  SIGNAL o1_inter,o2_inter,o3_inter,o4_inter : uint8;
  SIGNAL o1_post ,o2_post ,o3_post ,o4_post  : std_logic;
  SIGNAL vrle,vrle_pre,hrle,hrle_pre : std_logic;

  SIGNAL c0 : natural RANGE 0 TO 7;
  SIGNAL col_rgb : uv3;
--   aaa
--  b   c
--  b   c
--  b   c
--   ddd
--  e   f
--  e   f
--  e   f
--   ggg

  
  CONSTANT segments : arr_uv8(0 TO 15):=(
--   abcdefgh
    "11101110",  -- 0
    "00100100",  -- 1
    "10111010",  -- 2
    "10110110",  -- 3
    "01110100",  -- 4
    "11010110",  -- 5
    "11011110",  -- 6
    "10100100",  -- 7
    "11111110",  -- 8
    "11110110",  -- 9
    "00000000",  -- A
    "00000000",  -- B
    "00000000",  -- C
    "00000000",  -- D
    "00000000",  -- E
    "00000000"); -- F

--     0 1 2 3 4 5 6 7 8 9 1011
--     
--  0  ABABA A A A A A A A ACAC
--  1  ABABA A A A A A A A ACAC
--  2  B B                 C C
--  3  B B                 C C
--  4  B B                 C C
--  5  B B                 C C
--  6  B B                 C C
--  7  B B                 C C
--  8  B B                 C C
--  9  BDE D D D D D D D D CDF
-- 10  BDE D D D D D D D D CDF
-- 11  E E                 F F
-- 12  E E                 F F
-- 13  E E                 F F
-- 14  E E                 F F
-- 15  E E                 F F
-- 16  E E                 F F
-- 17  E E                 F F
-- 18  EGEGG G G G G G G G FGFG
-- 19  EGEGG G G G G G G G FGFG

 ------------------------------------------------
  FUNCTION objadrs(
    vpos : uint9; -- Spot vertical   position
    vc   : uv8; -- Vertical   coordinate object
    size : uv2; -- Object size
    base : uv8) RETURN unsigned IS
    VARIABLE a : uint8;
    VARIABLE ivc : natural := to_integer(vc);
  BEGIN
    CASE size IS
      WHEN "00" => -- Scale x1
        a:=(vpos-ivc)  MOD 16;
      WHEN "01" => -- Scale x2
        a:=((vpos-ivc)/2) MOD 16;
      WHEN "10" => -- Scale x4
        a:=((vpos-ivc)/4) MOD 16;
      WHEN OTHERS => -- Scale x8
        a:=((vpos-ivc)/8) MOD 16;
    END CASE;
    RETURN to_unsigned(a,8) + base;
  END FUNCTION;
  
  ------------------------------------------------
  -- First line below object
  FUNCTION objlast(
    vpos : uint9; -- Spot vertical   position
    vc   : uv8; -- Vertical   coordinate object
    hpos : uint9;
    hc   : uv8;
    size : uv2) RETURN boolean IS -- Object size
    VARIABLE ivc : uint8 := to_integer(vc);
    VARIABLE ihc : uint8 := to_integer(hc);
  BEGIN
    IF hc > 227 AND false THEN
      RETURN false;
    ELSE
      CASE size IS
        WHEN "00" => -- Scale x1
          RETURN vpos>=ivc AND (vpos-ivc)=9 AND hpos=220;
        WHEN "01" => -- Scale x2
          RETURN vpos>=ivc AND (vpos-ivc)=19 AND hpos=220;
        WHEN "10" => -- Scale x4
          RETURN vpos>=ivc AND (vpos-ivc)=39 AND hpos=220;
        WHEN OTHERS => -- Scale x8
          RETURN vpos>=ivc AND (vpos-ivc)=79 AND hpos=220;
      END CASE;
    END IF;
  END FUNCTION;
  
  ------------------------------------------------
  -- Bit selection in object pattern
  FUNCTION objbit(
    hpos : uint9; -- Spot horizontal position
    hc   : uv8; -- Horizontal coordinate object
    size : uv2) RETURN natural IS -- Object size
    VARIABLE a : uint3;
    VARIABLE ihc : uint8 := to_integer(hc);
  BEGIN
    CASE size IS
      WHEN "00" => -- Scale x1
        a:=(hpos-ihc) MOD 8;
      WHEN "01" => -- Scale x2
        a:=((hpos-ihc)/2) MOD 8;
      WHEN "10" => -- Scale x4
        a:=((hpos-ihc)/4) MOD 8;
      WHEN OTHERS => -- Scale x8
        a:=((hpos-ihc)/8) MOD 8;
    END CASE;
    RETURN 7-a;
  END FUNCTION;
  
  ------------------------------------------------
  -- Object hit
  FUNCTION objhit(
    hpos : uint9; -- Spot horizontal position
    vpos : uint9; -- Spot vertical   position
    hc   : uv8; -- Horizontal coordinate object
    vc   : uv8; -- Vertical   coordinate object
    size : uv2) RETURN boolean IS -- Object size
    VARIABLE ivc : uint8 := to_integer(vc);
    VARIABLE ihc : uint8 := to_integer(hc);
  BEGIN
    IF hc > 227 THEN
      RETURN false;
    ELSE
      CASE size IS
        WHEN "00" => -- Scale x1
          RETURN vpos>=ivc AND (vpos-ivc)<10 AND hpos>=ihc AND (hpos-ihc)<8;
        WHEN "01" => -- Scale x2
          RETURN vpos>=ivc AND (vpos-ivc)<20 AND hpos>=ihc AND (hpos-ihc)<16;
        WHEN "10" => -- Scale x4
          RETURN vpos>=ivc AND (vpos-ivc)<40 AND hpos>=ihc AND (hpos-ihc)<32;
        WHEN OTHERS => -- Scale x8
          RETURN vpos>=ivc AND (vpos-ivc)<80 AND hpos>=ihc AND (hpos-ihc)<64;
      END CASE;
    END IF;
  END FUNCTION;
  
  ------------------------------------------------
  FUNCTION bgmax(
    dr : uv8; 
    hpos : uint9;
    vpos20 : uint5;
    vbar   : uint5) RETURN boolean IS
    VARIABLE b : uint3;
    VARIABLE a : std_logic;
    VARIABLE r : boolean;
  BEGIN
    b:=(hpos-32) MOD 8;
    
    -- 0 : Extend bars of set 1 to 8 clocks
    -- 1 : Extend top 9 lines of set 2 to 8 clocks
    -- 2 : Extend bottom 9 lines of set 2 to 8 clocks
    -- 3 : Extend bars of set 3 to 8 clocks
    -- 4 : Extend top 9 lines of set 4 to 8 clocks
    -- 5 : Extend bottom 9 lines of set 4 to 8 clocks
    -- 7:6 : 00=x1 01=x2 10=x1 11=x4 for non selected lines
    
    IF vbar MOD 4=0 THEN
      a:=dr(0);
    ELSIF vpos20<11 AND vbar MOD 4=1 THEN
      a:=dr(1);
    ELSIF vbar MOD 4=1 THEN
      a:=dr(2);
    ELSIF vbar MOD 4=2 THEN
      a:=dr(3);
    ELSIF vpos20<11 AND vbar MOD 4=3 THEN
      a:=dr(4);
    ELSE
      a:=dr(5);
    END IF;
    
    IF a='1' THEN
      r:=true;
    ELSE
      CASE dr(7 DOWNTO 6) IS
        WHEN "00"   => r:=(b=0); -- 1 pixel
        WHEN "01"   => r:=(b<2); -- 2 pixels
        WHEN "10"   => r:=(b=0); -- 1 pixel
        WHEN OTHERS => r:=(b<4); -- 4 pixels
      END CASE;
    END IF;
    
    RETURN r;
  END FUNCTION;

  ------------------------------------------------
  -- 28 12 4 12 4 12 4 12
  --    ##   ##   ##   ##
  --    28   44   60   76

  -- 28 12 4 12   20   12 4 12
  --    ##   ##        ##   ##
  --    28   44        76   92
  
  FUNCTION score(
    hpos   : uint9;
    vpos   : uint9;
    s1_val : uv4;
    s2_val : uv4;
    s3_val : uv4;
    s4_val : uv4;
    s_pos  : std_logic;
    s_form : std_logic) RETURN boolean IS
    VARIABLE d   : natural RANGE 0 TO 3; -- DIGIT
    VARIABLE mask : uv8;
    VARIABLE val : uv4;
    VARIABLE a : boolean;
    VARIABLE h,v : uint8;
  BEGIN
    -- pos : 0= HIGH : 20V...39V 1= LOW = 200v .. 219V
    IF s_pos='0' AND vpos>=20 AND vpos<40 THEN
      v:=vpos-20;
      a:=true;
    ELSIF s_pos='1' AND vpos>=200 AND vpos<220 THEN
      v:=vpos-200;
      a:=true;
    ELSE
      v:=0;
      a:=false;
    END IF;

    IF s_form='1' THEN
      IF hpos>=32+28 AND hpos<32+28+12 THEN
        d:=0;
        h:=hpos-28-32;
      ELSIF hpos>=32+44 AND hpos<32+44+12 THEN
        d:=1;
        h:=hpos-44-32;
      ELSIF hpos>=32+60 AND hpos<32+60+12 THEN
        d:=2;
        h:=hpos-60-32;
      ELSIF hpos>=32+76 AND hpos<32+76+12 THEN
        d:=3;
        h:=hpos-76-32;
      ELSE
        a:=false;
        d:=0;
        h:=0;
      END IF;
    ELSE
      IF hpos>=32+28 AND hpos<32+28+12 THEN
        d:=0;
        h:=hpos-28-32;
      ELSIF hpos>=32+44 AND hpos<32+44+12 THEN
        d:=1;
        h:=hpos-44-32;
      ELSIF hpos>=32+76 AND hpos<32+76+12 THEN
        d:=2;
        h:=hpos-76-32;
      ELSIF hpos>=32+92 AND hpos<32+92+12 THEN
        d:=3;
        h:=hpos-92-32;
      ELSE
        a:=false;
        d:=0;
        h:=0;
      END IF;
    END IF;
    CASE d IS
      WHEN 0 => val:=s1_val;
      WHEN 1 => val:=s2_val;
      WHEN 2 => val:=s3_val;
      WHEN 3 => val:=s4_val;
    END CASE;
    
    mask(7):=to_std_logic(v<2); -- A
    mask(6):=to_std_logic(h<4 AND v<11); -- B
    mask(5):=to_std_logic(h>7 AND v<11); -- C
    mask(4):=to_std_logic(v>8 AND v<11); -- D
    mask(3):=to_std_logic(h<4 AND v>8);  -- E
    mask(2):=to_std_logic(h>7 AND v>8);  -- F
    mask(1):=to_std_logic(v>17); -- G
    mask(0):='0';
    
    IF (mask AND segments(to_integer(val)))=x"00" THEN
      a:=false;
    END IF;
    
    RETURN a;
    
  END FUNCTION;
  
  ------------------------------------------------
  FUNCTION orcol(hit : std_logic;
                 p   : unsigned(2 DOWNTO 0);
                 n   : unsigned(2 DOWNTO 0)) RETURN unsigned IS
  BEGIN
    IF hit='1' THEN
      RETURN p OR n;
    ELSE
      RETURN n;
    END IF;
  END FUNCTION;
  
BEGIN

  wreq<=wr AND req;
  
  dr<=dr_reg WHEN drreg_sel='1' ELSE dr_mem;
  
  Regs:PROCESS(clk,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
      pre_ocoll<='0';
      pre_coll<='0';
      
    ELSIF rising_edge(clk) THEN
      --------------------------------------------
      -- RAM
      dr_mem<=mem(to_integer(ad(7 DOWNTO 0)));
      IF wreq='1' THEN
        mem(to_integer(ad(7 DOWNTO 0)))<=dw;
      END IF;

      --------------------------------------------
      drreg_sel<='0';
      
      IF ad(10 DOWNTO 0)>="111" & x"C0" THEN
        drreg_sel<='1';
        CASE ad(3 DOWNTO 0) IS
          WHEN x"0" => dr_reg<=osize;  IF wreq='1' THEN  osize <=dw; END IF;
          WHEN x"1" => dr_reg<=ocol12; IF wreq='1' THEN  ocol12<=dw; END IF;
          WHEN x"2" => dr_reg<=ocol34; IF wreq='1' THEN  ocol34<=dw; END IF;
          WHEN x"3" => dr_reg<=spos;   IF wreq='1' THEN  spos  <=dw; END IF;
          WHEN x"4" => dr_reg<=x"00"; -- <undef>
          WHEN x"5" => dr_reg<=x"00"; -- <undef>
          WHEN x"6" => dr_reg<=bgcolour;  IF wreq='1' THEN bgcolour  <=dw; END IF;
          WHEN x"7" => dr_reg<=sper;   IF wreq='1' THEN sper   <=dw; END IF;
          WHEN x"8" => dr_reg<=sval12; IF wreq='1' THEN sval12 <=dw; END IF;
          WHEN x"9" => dr_reg<=sval34; IF wreq='1' THEN sval34 <=dw; END IF;
          WHEN x"A" => dr_reg<=bgcoll;
          WHEN x"B" => dr_reg<='0' & vrle & ocoll(5 DOWNTO 0);
          WHEN x"C" => dr_reg<=pot1;
          WHEN x"D" => dr_reg<=pot2;
          WHEN x"E" => dr_reg<=x"00"; -- <undef>
          WHEN x"F" => dr_reg<=x"00"; -- <undef>
          WHEN OTHERS => NULL;
        END CASE;

        ocol12(7 DOWNTO 6)<="00";
        ocol34(7 DOWNTO 6)<="00";
        spos  (7 DOWNTO 2)<="000000";
        bgcolour (7)<='0';
      ELSE
        IF wreq='1' THEN
          CASE ad(11 DOWNTO 0) IS
            WHEN x"70A" | x"F0A" => o1_hc <=dw;
            WHEN x"70B" | x"F0B" => o1_hcb<=dw;
            WHEN x"70C" | x"F0C" => o1_vc <=dw;
            WHEN x"70D" | x"F0D" => o1_vcb<=dw;
            WHEN x"71A" | x"F1A" => o2_hc <=dw;
            WHEN x"71B" | x"F1B" => o2_hcb<=dw;
            WHEN x"71C" | x"F1C" => o2_vc <=dw;
            WHEN x"71D" | x"F1D" => o2_vcb<=dw;
            WHEN x"72A" | x"F2A" => o3_hc <=dw;
            WHEN x"72B" | x"F2B" => o3_hcb<=dw;
            WHEN x"72C" | x"F2C" => o3_vc <=dw;
            WHEN x"72D" | x"F2D" => o3_vcb<=dw;
            WHEN x"74A" | x"F4A" => o4_hc <=dw;
            WHEN x"74B" | x"F4B" => o4_hcb<=dw;
            WHEN x"74C" | x"F4C" => o4_vc <=dw;
            WHEN x"74D" | x"F4D" => o4_vcb<=dw;
            WHEN OTHERS => NULL;
          END CASE;
        END IF;
      END IF;
      
      --------------------------------------------
      -- Clear collisions after register read
      bgcoll<=bgcoll OR (o1b_coll & o2b_coll & o3b_coll & o4b_coll &
                         o1_cplt & o2_cplt & o3_cplt & o4_cplt);
      
      IF (ad(11 DOWNTO 0)=x"FCA" OR ad(11 DOWNTO 0)=x"7CA") AND req='1'  THEN
        pre_coll<='1';
      END IF;
      
      clr_coll<='0';
      IF (ad(11 DOWNTO 0)/=x"FCA" AND ad(11 DOWNTO 0)/=x"7CA") AND pre_coll='1' THEN
        clr_coll<='1';
        pre_coll<='0';
      END IF;
      
      IF clr_coll='1' OR (vrle_pre='1' AND vrle='0') THEN
        bgcoll<=x"00";
      END IF;
      
      --------------------------------------------
      ocoll<=ocoll OR ("00" & o12_coll & o13_coll &
                       o14_coll & o23_coll & o24_coll & o34_coll);
      
      IF (ad(11 DOWNTO 0)=x"FCB" OR ad(11 DOWNTO 0)=x"7CB") AND req='1' THEN
        pre_ocoll<='1';
      END IF;
      
      clr_ocoll<='0';
      IF (ad(11 DOWNTO 0)/=x"FCB" AND ad(11 DOWNTO 0)/=x"7CB") AND pre_ocoll='1' THEN
        clr_ocoll<='1';
        pre_ocoll<='0';
      END IF;

      IF clr_ocoll='1' OR (vrle_pre='1' AND vrle='0') THEN
        ocoll<=x"00";
      END IF;
    
      --------------------------------------------
    END IF;
    
  END PROCESS Regs;


  Madar:PROCESS(clk) IS
  BEGIN
    IF rising_edge(clk) THEN
      mdrp<=mem(to_integer(mad));
    END IF;
  END PROCESS Madar;

  mdr<=mdrp WHEN rising_edge(clk);
  
  ------------------------------------------------------------------------------
  MadMad:PROCESS(c0,hpix,vbar,vpos,vpos1,vpos2,
                 o1_vcm,o1_vcbm,o1_diff,o1_size,o1_post,
                 o2_vcm,o2_vcbm,o2_diff,o2_size,o2_post,
                 o3_vcm,o3_vcbm,o3_diff,o3_size,o3_post,
                 o4_vcm,o4_vcbm,o4_diff,o4_size,o4_post) IS
  BEGIN
      CASE c0 IS
        WHEN 0 | 1 | 7 =>
          -- Vertical   background
          mad<=to_unsigned((hpix / 8 + vbar * 2),8) + x"80";
          
        WHEN 2 => 
          -- Horizontal background
          mad<=to_unsigned(vbar / 4,8) + x"A8";
          
        WHEN 3 => -- Object 1
          IF o1_post='0' THEN
            mad<=objadrs(vpos2,o1_vcm,o1_size,x"00");
          ELSE
            mad<=objadrs(o1_diff,o1_vcbm,o1_size,x"00");
          END IF;
          
        WHEN 4 => -- Object 2
          IF o2_post='0' THEN
            mad<=objadrs(vpos2,o2_vcm,o2_size,x"10");
          ELSE
            mad<=objadrs(o2_diff,o2_vcbm,o2_size,x"10");
          END IF;
          
        WHEN 5 => -- Object 3
          IF o3_post='0' THEN
            mad<=objadrs(vpos2,o3_vcm,o3_size,x"20");
          ELSE
            mad<=objadrs(o3_diff,o3_vcbm,o3_size,x"20");
          END IF;
          
        WHEN 6 => -- Object 4
          IF o4_post='0' THEN
            mad<=objadrs(vpos2,o4_vcm,o4_size,x"40");
          ELSE
            mad<=objadrs(o4_diff,o4_vcbm,o4_size,x"40");
          END IF;
          
        --WHEN 7 => -- Score
      END CASE;

  END PROCESS MadMad;
  
  ------------------------------------------------------------------------------
  Vid:PROCESS (clk,reset_na) IS
    VARIABLE b,h : boolean;
    VARIABLE i : natural RANGE 0 TO 7;
    VARIABLE o4_hit_v : std_logic;
  BEGIN
    IF reset_na='0' THEN
      int_i<='0';
      o1_cplt<='0';
      o2_cplt<='0';
      o3_cplt<='0';
      o4_cplt<='0';
    ELSIF rising_edge(clk) THEN
      --------------------------------------------
      --IF np='0' THEN
      --  -- NTSC
      --  htotal <=227;
      --  hsync<=224;
      --  hdisp<=222;
      --  vtotal <=262;
      --  vsync<=253;
      --  vdisp<=252;
      --ELSE
      --  -- PAL
      --  htotal <=284;
      --  hsync<=280;
      --  hdisp<=228;
      --  vtotal <=312;
      --  vsync<=260;
      --  vdisp<=252;
      --END IF;
      
      -- VRST  : 1 ... 43
      -- VSYNC : 12 ... 14
      -- TOTAL : 312
      vdisp     <=268;
      vsyncstart<=301;
      vsyncend  <=303;
      vtotal    <=312;

      -- HRST : 6 ... 49
      -- HSYNC : 11 ... 28
      -- TOTAL : 227
      hdisp     <=184;
      hsyncstart<=195;
      hsyncend  <=212;
      htotal    <=227;
      
      --------------------------------------------
      -- Collisions pulses
      o12_coll<='0';
      o23_coll<='0';
      o34_coll<='0';
      o13_coll<='0';
      o14_coll<='0';
      o24_coll<='0';
      o1b_coll<='0';
      o2b_coll<='0';
      o3b_coll<='0';
      o4b_coll<='0';
      
      o1_cplt<='0';
      o2_cplt<='0';
      o3_cplt<='0';
      o4_cplt<='0';

      hpulse<='0';
      --------------------------------------------
      c0<=(c0+1) MOD 8;
      
      CASE c0 IS
        WHEN 0 => -- Clear
          IF hpos<htotal-1 THEN
            hpos<=hpos+1;
          ELSE
            hpos<=0;
            IF vpos<vtotal-1 THEN
              vpos<=vpos+1;
              vpos1<=vpos;
              vpos2<=vpos1;
            ELSE
              vpos<=0;
              vpos1<=0;
              vpos2<=0;
            END IF;
            
            hpulse<='1';
            o1_diff<=(o1_diff+1) MOD 512;
            o2_diff<=(o2_diff+1) MOD 512;
            o3_diff<=(o3_diff+1) MOD 512;
            o4_diff<=(o4_diff+1) MOD 512;
            
            IF o1_diff_clr='1' THEN
              o1_diff<=0;
              o1_diff_clr<='0';
            END IF;
            IF o2_diff_clr='1' THEN
              o2_diff<=0;
              o2_diff_clr<='0';
            END IF;
            IF o3_diff_clr='1' THEN
              o3_diff<=0;
              o3_diff_clr<='0';
            END IF;
            IF o4_diff_clr='1' THEN
              o4_diff<=0;
              o4_diff_clr<='0';
            END IF;
            IF vpos<20 THEN
              --valt<='0';
              vpos20<=0;
              vbar<=0;
            ELSE
              IF vpos20<19 THEN
                vpos20<=vpos20+1;
              ELSE
                vpos20<=0;
              END IF;
              IF vpos20=1 OR vpos20=19 THEN
                vbar<=vbar+1;
              END IF;
            END IF;
          END IF;
          
          hpix<=((hpos-31) MOD 128)/8;

          ------------------------------------------------------
          IF vpos=0 THEN
            o1_post<='0';
            o2_post<='0';
            o3_post<='0';
            o4_post<='0';
            o1_diff_clr<='0';
            o2_diff_clr<='0';
            o3_diff_clr<='0';
            o4_diff_clr<='0';
            
          END IF;
          
          ------------------------------------------------------
          IF o4_post='0' THEN
            IF objlast(vpos2,o4_vcm,hpos,o4_hc,o4_size) THEN
              o4_post<='1';
              o4_cplt<='1';
              o4_diff_clr<='1';
              o4_vcbm<=o4_vcb+1;
            END IF;
            i:=objbit(hpos,o4_hc,o4_size);
            h:=objhit(hpos,vpos2,o4_hc,o4_vcm,o4_size);
          ELSE
            IF objlast(o4_diff,o4_vcbm,hpos,o4_hcbm,o4_size) THEN
              o4_cplt<='1';
              o4_diff_clr<='1';
              o4_vcbm<=o4_vcb+1;
            END IF;
            i:=objbit(hpos,o4_hcbm,o4_size);
            h:=objhit(hpos,o4_diff,o4_hcbm,o4_vcbm,o4_size);
          END IF;
          
          o4_hit_v:='0';
          IF h AND mdr(i)='1' THEN -- HIT
            o4_hit<='1';
            o4_hit_v:='1';
            vid_rgb<=orcol(o1_hit OR o2_hit OR o3_hit,
                           col_rgb,NOT o4_col);
          ELSE
            vid_rgb<=col_rgb;
          END IF;
          
          IF score(hpos,vpos,
                   s1_val,s2_val,s3_val,s4_val,s_pos,s_form) THEN
            vid_rgb<=NOT bg_colour;
          END IF;
          
          ------------------------------------------------------
          o12_coll<=o1_hit   AND o2_hit;
          o23_coll<=o2_hit   AND o3_hit;
          o34_coll<=o3_hit   AND o4_hit_v;
          o13_coll<=o1_hit   AND o3_hit;
          o14_coll<=o1_hit   AND o4_hit_v;
          o24_coll<=o2_hit   AND o4_hit_v;
          o1b_coll<=o1_hit   AND bg_hit;
          o2b_coll<=o2_hit   AND bg_hit;
          o3b_coll<=o3_hit   AND bg_hit;
          o4b_coll<=o4_hit_v AND bg_hit;
          
        WHEN 1 =>
          -- Vertical   background
            col_rgb<=sc_colour; -- Screen colour if no object, no BG
            o1_hit<='0';
            o2_hit<='0';
            o3_hit<='0';
            o4_hit<='0';
            bg_hit<='0';
          
        WHEN 2 => 
          -- Horizontal background
          NULL;
          
        WHEN 3 => -- Background
          bg_dr<=mdr;
          
        WHEN 4 => -- Background
          b:=bg_dr(7-(hpix MOD 8))='1'; -- Hit Vertical
          h:=(hpos>=32) AND (hpos<32+128) AND
             (vpos>=20) AND (vpos<20+200); -- Zone affichage fond
          
          IF h AND b AND bgmax(mdr,hpos,vpos20,vbar) AND bg_ena='1' THEN
            col_rgb<=bg_colour;   -- Background colour
            bg_hit<='1';
          END IF;
          
        WHEN 5 => -- Object 1
          IF o1_post='0' THEN
            IF objlast(vpos2,o1_vcm,hpos,o1_hc,o1_size) THEN
              o1_post<='1';
              o1_cplt<='1';
              o1_diff_clr<='1';
              o1_vcbm<=o1_vcb+1;
            END IF;
            i:=objbit(hpos,o1_hc,o1_size);
            h:=objhit(hpos,vpos2,o1_hc,o1_vcm,o1_size);
          ELSE
            IF objlast(o1_diff,o1_vcbm,hpos,o1_hcbm,o1_size) THEN
              o1_cplt<='1';
              o1_diff_clr<='1';
              o1_vcbm<=o1_vcb+1;
            END IF;
            i:=objbit(hpos,o1_hcbm,o1_size);
            h:=objhit(hpos,o1_diff,o1_hcbm,o1_vcbm,o1_size);
          END IF;
          IF h AND mdr(i)='1' THEN -- HIT
            col_rgb<=NOT o1_col;
            o1_hit<='1';
          END IF;
          
        WHEN 6 => -- Object 2
          IF o2_post='0' THEN
            IF objlast(vpos2,o2_vcm,hpos,o2_hc,o2_size) THEN
              o2_post<='1';
              o2_cplt<='1';
              o2_diff_clr<='1';
              o2_vcbm<=o2_vcb+1;
            END IF;
            i:=objbit(hpos,o2_hc,o2_size);
            h:=objhit(hpos,vpos2,o2_hc,o2_vcm,o2_size);
          ELSE
            IF objlast(o2_diff,o2_vcbm,hpos,o2_hcbm,o2_size) THEN
              o2_cplt<='1';
              o2_diff_clr<='1';
              o2_vcbm<=o2_vcb+1;
            END IF;
            i:=objbit(hpos,o2_hcbm,o2_size);
            h:=objhit(hpos,o2_diff,o2_hcbm,o2_vcbm,o2_size);
          END IF;
          IF h AND mdr(i)='1' THEN -- HIT
            col_rgb<=orcol(o1_hit,col_rgb,NOT o2_col);
            o2_hit<='1';
          END IF;
          
       WHEN 7 => -- Object 3
          IF o3_post='0' THEN
            IF objlast(vpos2,o3_vcm,hpos,o3_hc,o3_size) THEN
              o3_post<='1';
              o3_cplt<='1';
              o3_diff_clr<='1';
              o3_vcbm<=o3_vcb+1;
            END IF;
            i:=objbit(hpos,o3_hc,o3_size);
            h:=objhit(hpos,vpos2,o3_hc,o3_vcm,o3_size);
          ELSE
            IF objlast(o3_diff,o3_vcbm,hpos,o3_hcbm,o3_size) THEN
              o3_cplt<='1';
              o3_diff_clr<='1';
              o3_vcbm<=o3_vcb+1;
            END IF;
            i:=objbit(hpos,o3_hcbm,o3_size);
            h:=objhit(hpos,o3_diff,o3_hcbm,o3_vcbm,o3_size);
          END IF;
          IF h AND mdr(i)='1' THEN -- HIT
            col_rgb<=orcol(o1_hit OR o2_hit,col_rgb,NOT o3_col);
            o3_hit<='1';
          END IF;
            
      END CASE;
    
      --------------------------------------------
      
      --CASE c0 IS
      --  WHEN 1 => -- Clear
      --  WHEN 2 => NULL;
      --  WHEN 3 => -- Vertical   background
      --  WHEN 4 => -- Horizontal background
      --  WHEN 5 => -- Object 1
      --  WHEN 6 => -- Object 2
      --  WHEN 7 => -- Object 3
      --  WHEN 0 => -- Object 4
      --END CASE;

      IF hrle='1' AND hrle_pre='0' THEN
        o1_hcbm<=o1_hcb;
        o2_hcbm<=o2_hcb;
        o3_hcbm<=o3_hcb;
        o4_hcbm<=o4_hcb;
      END IF;
      IF vrle='0' AND vrle_pre='1' THEN
        o1_vcm<=o1_vc;
        o2_vcm<=o2_vc;
        o3_vcm<=o3_vc;
        o4_vcm<=o4_vc;
      END IF;
      
      vid_hsyn<=to_std_logic(hpos>=hsyncstart AND hpos<hsyncend);
      vid_vsyn<=to_std_logic(vpos>=vsyncstart AND hpos<vsyncend);
      
      vrle    <=to_std_logic(vpos>=vdisp);
      vrle_pre<=vrle;
      
      hrle    <=to_std_logic(hpos>=hdisp);
      hrle_pre<=hrle;
      
      vid_de  <=to_std_logic(hpos<hdisp AND vpos<vdisp);
      
      --------------------------------------------
      
      vid_ce<=tick_divi;
      --------------------------------------------
      o1_cpltm<=(o1_cpltm OR o1_cplt) AND NOT (hrle AND NOT hrle_pre);
      o2_cpltm<=(o2_cpltm OR o2_cplt) AND NOT (hrle AND NOT hrle_pre);
      o3_cpltm<=(o3_cpltm OR o3_cplt) AND NOT (hrle AND NOT hrle_pre);
      o4_cpltm<=(o4_cpltm OR o4_cplt) AND NOT (hrle AND NOT hrle_pre);
      
      int_i<=
        (int_i OR
         ((o1_cpltm OR o2_cpltm OR o3_cpltm OR o4_cpltm) AND hrle AND NOT hrle_pre) OR
         (vrle AND NOT vrle_pre))
        AND NOT intack; -- AND NOT (NOT vrle AND vrle_pre);
    --------------------------------------------
    END IF;


  END PROCESS Vid;

  ------------------------------------------------------------------------------
  DivCLK:PROCESS (clk,reset_na) IS
  BEGIN
    IF reset_na='0' THEN
      tick_divi<='0';
    ELSIF rising_edge(clk) THEN
      IF divi=7 THEN
        divi<=0;
        tick_divi<='1';
    ELSE
        divi<=divi+1;
        tick_divi<='0';
      END IF;
    END IF;
  END PROCESS DivCLK;
  
  ------------------------------------------------------------------------------
  Sono:PROCESS(clk) IS
  BEGIN

    IF rising_edge(clk) THEN
      IF hpulse='1' THEN
        IF snd_cpt<sper THEN
          snd_cpt<=snd_cpt+1;
        ELSE
          snd_cpt<=x"00";
          sound_i<=NOT sound_i;
        END IF;
      END IF;
      
      IF sper=x"00" THEN
        sound_i<='0';
      END IF;
      
    END IF;
  END PROCESS Sono;
  
  sound<=x"7F" WHEN sound_i='0' ELSE x"80";
  
  ack<='1';
  int<=int_i;
  ivec<=x"03";
  
END ARCHITECTURE rtl;

