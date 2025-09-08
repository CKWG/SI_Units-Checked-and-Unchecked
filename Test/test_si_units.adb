------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2006, 2008, 2018, 2020, 2022, 2025
-- Christoph Karl Walter Grein
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 3
-- of the License, or any later version.
--
-- This program is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
-- GNU General Public License for more details.
--
-- You should have received a copy of the GNU General Public License
-- along with this program; if not, write to the Free Software
-- Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA  02111-1307, USA.
--
-- As a special exception,  if other files  instantiate  generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License.  This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Author's email address:
--   christ-Usch.grein@t-online.de
------------------------------------------------------------------------------

with Test_Support;
use  Test_Support;

with SI.Strings;
use  SI.Strings, SI;

with Ada.Exceptions;
with Ada.Strings.Fixed;

procedure Test_SI_Units is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   5.1
  -- Date      12 August 2025
  --====================================================================
  -- Test that all derived symbols are correctly defined.
  -- Test that various incorrect symbols are deteced.
  -- Test the prefix values.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  10.07.2002
  --  C.G.    1.1  06.08.2002 Test_OK renamed
  --  C.G.    2.0  15.02.2006 Dim parameter added
  --  C.G.    2.1  03.04.2008 Unit names like "V"_p_Meter, Meter_3
  --                          do no longer exist
  --  C.G.    3.0  29.07.2018 String units; test completely rewritten
  --  C.G.    3.1  31.08.2018 Dimensionless ""
  --  C.G.    3.2  15.11.2018 Bug fix in test ("/(s/m)" was treated as
  --                          legal, but / not allowed in denominator)
  --  C.G.    4.0  13.05.2020 Dimensions generic parameter
  --  C.G.    4.1  24.07.2020 Added prefix tests
  --  C.G.    4.2  05.08.2020 Added tests like 1.0/"unit"
  --  C.G.    4.3  09.08.2020 Added eV
  --  C.G.    4.4  06.12.2022 prefixes QqRr added
  --  C.G.    4.5  11.04.2025 Added ha
  --  C.G.    5.0  11.08.2025 Better systematic tests for new implemen-
  --                          tation of unit string evaluation
  --  C.G.    5.1  12.08.2025 Make test succeed in Unchecked variant
  --====================================================================

  function Test_Checked return Boolean is
    T: Time;
  begin
    T := 1.0*"S";
    return False;
  exception
    when others =>
      return True;
  end Test_Checked;

  is_Checked: Boolean renames Test_Checked;

  procedure Test_Prefix is
    subtype Index is Natural range 0 .. 5;
    type PU_Str (Length: Index := 0) is record
      PoU: String (1 .. Length);
      Val: Float;
    end record;
    PL: constant array (-12 .. 12) of PU_Str :=
      (-12 => (1, "q" , 1.0E-30),
       +12 => (1, "Q" , 1.0E+30),
       -11 => (1, "r" , 1.0E-27),
       +11 => (1, "R" , 1.0E+27),
       -10 => (1, "y" , 1.0E-24),
       +10 => (1, "Y" , 1.0E+24),
        -9 => (1, "z" , 1.0E-21),
        +9 => (1, "Z" , 1.0E+21),
        -8 => (1, "a" , 1.0E-18),
        +8 => (1, "E" , 1.0E+18),
        -7 => (1, "f" , 1.0E-15),
        +7 => (1, "P" , 1.0E+15),
        -6 => (1, "p" , 1.0E-12),
        +6 => (1, "T" , 1.0E+12),
        -5 => (1, "n" , 1.0E-09),
        +5 => (1, "G" , 1.0E+09),
        -4 => (1, "u" , 1.0E-06),
        +4 => (1, "M" , 1.0E+06),
        -3 => (1, "m" , 1.0E-03),
        +3 => (1, "k" , 1.0E+03),
        -2 => (1, "c" , 1.0E-02),
        +2 => (1, "h" , 1.0E+02),
        -1 => (1, "d" , 1.0E-01),
        +1 => (2, "da", 1.0E+01),
         0 => (0, ""  , 1.0E+00));
    UL: constant array (1 .. 31) of PU_Str :=
      ( 1 => (1, "A"  , 1.0),
        2 => (2, "Bq" , 1.0),
        3 => (1, "C"  , 1.0),
        4 => (1, "F"  , 1.0),
        5 => (2, "Gy" , 1.0),
        6 => (1, "H"  , 1.0),
        7 => (2, "Hz" , 1.0),
        8 => (1, "J"  , 1.0),
        9 => (1, "K"  , 1.0),
       10 => (1, "L"  , 1.0),
       11 => (1, "N"  , 1.0),
       12 => (3, "Ohm", 1.0),
       13 => (2, "Pa" , 1.0),
       14 => (1, "S"  , 1.0),
       15 => (2, "Sv" , 1.0),
       16 => (1, "T"  , 1.0),
       17 => (1, "V"  , 1.0),
       18 => (1, "W"  , 1.0),
       19 => (2, "Wb" , 1.0),
       20 => (2, "cd" , 1.0),
       21 => (2, "eV" , 1.0),
       22 => (1, "g"  , 1.0),
     --      (1, "h"  , 1.0),  no prefixes
     --      (2, "ha" , 1.0),  no prefixes
       23 => (3, "kat", 1.0),
       24 => (1, "l"  , 1.0),
       25 => (2, "lm" , 1.0),
       26 => (2, "lx" , 1.0),
       27 => (1, "m"  , 1.0),
     --      (3, "min", 1.0),  no prefixes
       28 => (3, "mol", 1.0),
       29 => (3, "rad", 1.0),
       30 => (1, "s"  , 1.0),
       31 => (2, "sr" , 1.0));
  begin
    for U in UL'Range loop
      for P in PL'Range loop  -- 1.0    * ("Q"        & "m"       ) = 1.0E24     * "m"
        Assert (Condition => UL (U).Val * (PL (P).PoU & UL (U).PoU) = PL (P).Val * UL (U).PoU,
                Message   => PL (P).PoU & UL (U).PoU & " is correct",
                Only_Report_Error => True);
      end loop;
    end loop;
  end Test_Prefix;

  type Operator is ('*', '/');

  procedure Test_Case (Op: Operator; Unit_String: String; Text: String;
                       Expect: Boolean; Expected_Result: String := "") is
    X: Item;
    use Ada.Strings.Fixed;
    function "-" (X: String) return String      -- Omit the unit string
      with Inline, Pre => X'First = 1 is        -- of the expected result
      Pos: constant Natural := Index (X, "*");  -- in the Unchecked variant
    begin                                       -- because it doesn't exist
      if is_Checked or Pos = 0 then             -- in the resulting image.
        return X;
      else
        return X (1 .. Pos - 1);
      end if;
    end "-";
  begin
    case Op is
      when '*' => X := 1.0 * Unit_String;
      when '/' => X := 1.0 / Unit_String;
    end case;
    Assert (Condition => Expect,
            Message   => Op'Image & '"' & Unit_String & '"' & (20 - Unit_String'Length) * ' ' & Text,
            Only_Report_Error => False);
    Assert (Condition => Image (X) = -Expected_Result,
            Message   => Image (X),
            Only_Report_Error => True);
  exception
    when E: Illegal_Unit =>
      Assert (Condition => not Expect,
              Message   => Op'Image & '"' & Unit_String & '"' & (20 - Unit_String'Length) * ' ' &
                           Text & (44 - Text'Length) * ' ' & "Illegal_Unit => " & Ada.Exceptions.Exception_Message (E),
              Only_Report_Error => False);
  end Test_Case;

begin

  Test_Header (Title => "Test SI_Units",
               Description => "Test syntax of unit strings and definitions for correctness.");

  Put_Line ("This is the " & (if is_Checked then "C" else "Unc") & "hecked variant." );

  Test_Step (Title => "Prefixes",
             Description => "Test all combinations of prefixes and symbols.");

  Test_Prefix;

  Test_Step (Title => "Syntactically correct and incorrect Symbols",
             Description => "Try to systematically check all possibilities.");
  -- Unit combinations below are mostly nonsense; important is the syntax only.

  Put_Line ("Dimensionless");
  Put_Line ("-------------");
  New_Line;
  Test_Case ('*', ""      , "dimensionless"                , Expect => True, Expected_Result => " 1.00000E+00");
  Test_Case ('*', " "     , "space instead of empty"       , Expect => False);
  Test_Case ('/', ""      , "dimensionless in denominator" , Expect => False);

  Put_Line ("Product");
  Put_Line ("-------");
  New_Line;
  Test_Case ('*', "*"              , "no letter"                 , Expect => False);
  Test_Case ('*', "L"              , "Liter"                     , Expect => True , Expected_Result => " 1.00000E-03*m**3");
  Test_Case ('*', "ml"             , "Milliliter"                , Expect => True , Expected_Result => " 1.00000E-06*m**3");
  Test_Case ('*', " ml"            , "leading space"             , Expect => False);
  Test_Case ('*', "l,"             , "any wrong character"       , Expect => False);
  Test_Case ('*', "da*m"           , "Prefix with multiplication", Expect => False);
  Test_Case ('*', "2*m"            , "value before symbol"       , Expect => False);
  Test_Case ('*', "m*2"            , "value after symbol"        , Expect => False);
  Test_Case ('*', "m2"             , "value in symbol"           , Expect => False);
  Test_Case ('*', "A "             , "any wrong character"       , Expect => False);
  Test_Case ('*', "A b"            , "space inside"              , Expect => False);
  Test_Case ('*', "damol"          , "Dekamol"                   , Expect => True , Expected_Result => " 1.00000E+01*mol");
  Test_Case ('*', "daOhmi"         , "too long"                  , Expect => False);
  Test_Case ('*', "A*s"            , "two factors"               , Expect => True , Expected_Result => " 1.00000E+00*s*A");
  Test_Case ('*', "A*s "           , "ending with space"         , Expect => False);
  Test_Case ('*', "km*(hl*N)"      , "no parentheses"            , Expect => False);
  Test_Case ('*', "V*S*dakat"      , "three factors"             , Expect => True , Expected_Result => " 1.00000E+01*s**(-1)*A*mol");
  Test_Case ('*', "mGy*Sv*zyxwvuUV", "third too long"            , Expect => False);

  Put_Line ("Exponent");  -- I doubt there are ever exponents with more than one digit - nevertheless, I test it.
  Put_Line ("--------");
  New_Line;
  Test_Case ('*', "cm**3"               , "simple exponent"               , Expect => True , Expected_Result => " 1.00000E-06*m**3");
  Test_Case ('*', "cm**11"              , "two-digit exponent"            , Expect => True , Expected_Result => " 1.00000E-22*m**11");
  Test_Case ('*', "cm**"                , "exponent missing"              , Expect => False);
  Test_Case ('*', "cm**m"               , "letter instead of digit"       , Expect => False);
  Test_Case ('*', "Mm* *"               , "space in exponentiation"       , Expect => False);
  Test_Case ('*', "m-2"                 , "Missing **("                   , Expect => False);
  Test_Case ('*', "s**-2"               , "Missing ("                     , Expect => False);
  Test_Case ('*', "dK**4*mrad"          , "factor after exponent"         , Expect => True , Expected_Result => " 1.00000E-07*K**4");
  Test_Case ('*', "nF**2*hm"            , "factor after exponent"         , Expect => True , Expected_Result => " 1.00000E-16*m**(-3)*kg**(-2)*s**8*A**4");
  Test_Case ('*', "K**87*"              , "letter missing in second"      , Expect => False);  -- quite nonsence
  Test_Case ('*', "K**87*5"             , "digit instead of letter"       , Expect => False);  -- exponents, dont' do
  Test_Case ('*', "K**87*;"             , "any instead of letter"         , Expect => False);  -- this with prefixes!
  Test_Case ('*', "s**(4)"              , "(simple exponent)"             , Expect => True , Expected_Result => " 1.00000E+00*s**4");
  Test_Case ('*', "s**(4"               , "closing ) missing"             , Expect => False);
  Test_Case ('*', "s**4)"               , "opening ( missing"             , Expect => False);
  Test_Case ('*', "ds**(4)*dam"         , "factor after (simple exponent)", Expect => True , Expected_Result => " 1.00000E-03*m*s**4");
  Test_Case ('*', "s**(4))*m"           , ") ending nonexistent divisor"  , Expect => False);
  Test_Case ('*', "s**(4*b"             , "closing ) missing"             , Expect => False);
  Test_Case ('*', "s**(4(*b"            , "wrong closing ("               , Expect => False);
  Test_Case ('*', "s**(+4)*C"           , "signed exponent"               , Expect => True , Expected_Result => " 1.00000E+00*s**5*A");
  Test_Case ('*', "s**(-4)*A"           , "signed exponent"               , Expect => True , Expected_Result => " 1.00000E+00*s**(-4)*A");
  Test_Case ('*', "s**(2+3)"            , "sum in exponent"               , Expect => False);
  Test_Case ('*', "s*cm**(-1/3)"        , "fractional exponent"           , Expect => True , Expected_Result => " 4.64159E+00*m**(-1/3)*s");
  Test_Case ('*', "s**5*cm**3"          , "two exponents"                 , Expect => True , Expected_Result => " 1.00000E-06*m**3*s**5");
  Test_Case ('*', "s**(+2/5)*cm**(-1/3)", "two fractional exponents"      , Expect => True , Expected_Result => " 4.64159E+00*m**(-1/3)*s**(2/5)");
  Test_Case ('*', "s**(4/-3)*cm"        , "sign in denominator"           , Expect => False);
  Test_Case ('*', "s**( -4/3)*b"        , "space before sign"             , Expect => False);
  Test_Case ('*', "s**(- 4/3)*b"        , "space after sign"              , Expect => False);
  Test_Case ('*', "s**(-4 /3)*b"        , "space before /"                , Expect => False);
  Test_Case ('*', "s**(-4/ 3)*b"        , "space after /"                 , Expect => False);
  Test_Case ('*', "s**(-4/3 )*b"        , "space before )"                , Expect => False);
  Test_Case ('*', "S**(/3)"             , "missing numerator in exponent" , Expect => False);
  Test_Case ('*', "S**(4/)"             , "missing denomiator in exponent", Expect => False);

  Put_Line ("Quotient");
  Put_Line ("--------");
  New_Line;
  Test_Case ('*', "/min"             , "factor missing"               , Expect => False);
  Test_Case ('*', "min/"             , "divisor missing"              , Expect => False);
  Test_Case ('*', "mol/min"          , "simple divisor"               , Expect => True , Expected_Result => " 1.66667E-02*s**(-1)*mol");
  Test_Case ('*', "mol/+"            , "sign in divisor"              , Expect => False);
  Test_Case ('*', "mol/ "            , "space after /"                , Expect => False);
  Test_Case ('*', "mol/9"            , "digit in divisor"             , Expect => False);
  Test_Case ('*', "J/(Wb)"           , "(simple divisor)"             , Expect => True , Expected_Result => " 1.00000E+00*A");
  Test_Case ('*', "J/Wb)"            , "opening ( missing"            , Expect => False);
  Test_Case ('*', "kJ/s*min"         , "missing ()"                   , Expect => False);
  Test_Case ('*', "kJ/(s*min)"       , "(product)"                    , Expect => True , Expected_Result => " 1.66667E+01*m**2*kg*s**(-4)");
  Test_Case ('*', "kJ/(s"            , ") missing"                    , Expect => False);
  Test_Case ('*', "kJ/(s *min)"      , "space inside"                 , Expect => False);
  Test_Case ('*', "kJ/(s*min) "      , "space at end"                 , Expect => False);
  Test_Case ('*', "kg/(m/s)"         , "/ in divisor"                 , Expect => False);
  Test_Case ('*', "m/(s)*kg"         , "factor after divisor"         , Expect => False);
  Test_Case ('*', "lm/(T**(-4))"     , "(divisor) ends"               , Expect => True , Expected_Result => " 1.00000E+00*kg**4*s**(-8)*A**(-4)*cd");
  Test_Case ('*', "lm/T**(-4))"      , ") after exponent"             , Expect => False);
  Test_Case ('*', "lm/(T**(-4)"      , "(divisor unclosed"            , Expect => False);
  Test_Case ('*', "lm/(T**(-4)+"     , "syntax character after (divisor"              , Expect => False);
  Test_Case ('*', "lm/(T**(-4))*h"   , "factor after (divisor)"       , Expect => False);
  Test_Case ('*', "lm/(T)!"          , "any after divisor"            , Expect => False);
  Test_Case ('*', "K**123/km"        , "divisor after simple exponent", Expect => True , Expected_Result => " 1.00000E-03*m**(-1)*K**123");
  Test_Case ('*', "m/K**123/km"      , "divisor in divisor"           , Expect => False);
  Test_Case ('*', "s**(4)/m**4"      , "divisor after simple exponent", Expect => True , Expected_Result => " 1.00000E+00*m**(-4)*s**4");
  Test_Case ('*', "s**(4)/m**4*kg"   , "missing ("                    , Expect => False);
  Test_Case ('*', "sr/T**4"          , "simple exponent in divisor"   , Expect => True , Expected_Result => " 1.00000E+00*kg**(-4)*s**8*A**4");
  Test_Case ('*', "sr/(T**3)"        , "(simple exponent) in divisor" , Expect => True , Expected_Result => " 1.00000E+00*kg**(-3)*s**6*A**3");
  Test_Case ('*', "sr/(T**2"         , ") missing in divisor"         , Expect => False);
  Test_Case ('*', "sr/(T**1*mol)"    , "(product with exponent)"      , Expect => True , Expected_Result => " 1.00000E+00*kg**(-1)*s**2*A*mol**(-1)");
  Test_Case ('*', "ukat/Pa**(-9)"    , "signed exponent in divisor"   , Expect => True , Expected_Result => " 1.00000E-06*m**(-9)*kg**9*s**(-19)*mol");
  Test_Case ('*', "ukat/Pa**(-9)/m"  , "divisor after (divisor)"      , Expect => False);
  Test_Case ('*', "ukat/(Pa**(-9))"  , "(signed exponent) in divisor" , Expect => True , Expected_Result => " 1.00000E-06*m**(-9)*kg**9*s**(-19)*mol");
  Test_Case ('*', "ukat/(Pa**(-9));" , "any after divisor"            , Expect => False);
  Test_Case ('*', "m/(s**(+2))*s;"   , "factor after divisor"         , Expect => False);
  Test_Case ('*', "mS**0/(daBq**0*l)", "(product)"                    , Expect => True , Expected_Result => " 1.00000E+03*m**(-3)");
  Test_Case ('*', "mS**0/(daBq**0**)", "Letter expected"              , Expect => False);

  Put_Line ("Divisor");
  Put_Line ("-------");
  New_Line;
  Test_Case ('/', "a"        , "no unit are"     , Expect => False);
  Test_Case ('/', "ha"       , "hectare"         , Expect => True , Expected_Result => " 1.00000E-04*m**(-2)");
  Test_Case ('/', ";"        , "no letter"       , Expect => False);     --
  Test_Case ('/', "ha*"      , "factor missing"  , Expect => False);
  Test_Case ('/', "(lx)"     , "(divisor)"       , Expect => False);
  Test_Case ('/', "Gs*g"     , "product"         , Expect => True , Expected_Result => " 1.00000E-06*kg**(-1)*s**(-1)");
  Test_Case ('/', "kat* rsto", "space in product", Expect => False);

  Put_Line ("No Prefix");
  Put_Line ("---------");
  New_Line;
  Test_Case ('*', "hh"  , "no prefix on h"  , Expect => False);
  Test_Case ('*', "daha", "no prefix on ha" , Expect => False);
  Test_Case ('*', "Mkg" , "no prefix on kg" , Expect => False);
  Test_Case ('*', "mmin", "no prefix on min", Expect => False);

  Test_Step (Title => "Final Test",
             Description => "The constant part of the Schottky-Langmuir equation.");

  Assert (Condition => 2.33395*10.0**(-6)*"m**(-3)*kg**(-3/2)*s**(9/2)*A**(5/2)" -
                       7.38060E-08*"dam**(-3)*hs**(9/2)*dag**(-3/2)*mA**(5/2)" <
                       1.0E-12*"m**(-3)*kg**(-3/2)*s**(9/2)*A**(5/2)",
          Message   => "Values are nearly equal",
          Only_Report_Error => False);

  Assert (Condition => Image (2.33395*10.0**(-6)*"m**(-3)*kg**(-3/2)*s**(9/2)*A**(5/2)") =
                       Image (7.38060E-08*"dam**(-3)*hs**(9/2)*dag**(-3/2)*mA**(5/2)" ),
          Message   => "Images are equal",
          Only_Report_Error => False);

  Test_Result;

end Test_SI_Units;














