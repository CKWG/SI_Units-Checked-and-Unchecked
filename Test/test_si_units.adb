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
-- Source:
-- https://www.adaic.org/ada-resources/tools-libraries/
--   (see Christoph Grein's Essentials)
-- http://archive.adaic.com/tools/CKWG/Dimension/Dimension.html
--
-- Author's email address:
--   christ-Usch.grein@t-online.de
------------------------------------------------------------------------------

with Test_Support;
use  Test_Support;

with SI.Strings;
use  SI.Strings, SI;

with Ada.Strings.Fixed;

procedure Test_SI_Units is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   4.5
  -- Date      11 April 2025
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
  --====================================================================

  procedure Test_OK (X: Item; Legal_Symbol: String; Text: String) is
    use Ada.Strings.Fixed;
  begin
    Assert (Condition => has_Dimension (X, Legal_Symbol),
            Message   => Text & (25 - Text'Length) * ' ' & Image (X),
            Only_Report_Error => False);
  end Test_OK;

  procedure Test_KO (Illegal_Symbol: String; Text: String) is
    X: Item;
    use Ada.Strings.Fixed;
  begin
    X := 1.0 * Illegal_Symbol;
    Assert (Condition => False,
            Message   => Text & " should have raised Illegal_Unit",
            Only_Report_Error => False);
  exception
    when Illegal_Unit =>
      Assert (Condition => True,
              Message   => Illegal_Symbol & ": " & (8 - Illegal_Symbol'Length) * ' ' &
                           Text & (44 - Text'Length) * ' ' & "Illegal_Unit",
              Only_Report_Error => False);
  end Test_KO;

  procedure Test_Prefix (PF: String; Value: Float) is
  begin
    Assert (Condition => 1.0*(PF&"m") = Value*"m",
            Message => PF & " is correct",
            Only_Report_Error => True);
  end Test_Prefix;

begin

  Test_Header (Title => "Test SI_Units",
               Description => "Test that all derived symbols are correctly defined.");

  Test_Step (Title => "Prefixes",
             Description => "Prefix values.");

  Test_Prefix ("q", 1.0E-30);
  Test_Prefix ("Q", 1.0E+30);
  Test_Prefix ("r", 1.0E-27);
  Test_Prefix ("R", 1.0E+27);
  Test_Prefix ("y", 1.0E-24);
  Test_Prefix ("Y", 1.0E+24);
  Test_Prefix ("z", 1.0E-21);
  Test_Prefix ("Z", 1.0E+21);
  Test_Prefix ("a", 1.0E-18);
  Test_Prefix ("E", 1.0E+18);
  Test_Prefix ("f", 1.0E-15);
  Test_Prefix ("P", 1.0E+15);
  Test_Prefix ("p", 1.0E-12);
  Test_Prefix ("T", 1.0E+12);
  Test_Prefix ("n", 1.0E-9);
  Test_Prefix ("G", 1.0E+9);
  Test_Prefix ("u", 1.0E-6);
  Test_Prefix ("M", 1.0E+6);
  Test_Prefix ("m", 1.0E-3);
  Test_Prefix ("k", 1.0E+3);
  Test_Prefix ("c", 1.0E-2);
  Test_Prefix ("h", 1.0E+2);
  Test_Prefix ("d", 1.0E-1);
  Test_Prefix ("da", 1.0E+1);

  Test_Step (Title => "Correct Symbols",
             Description => "The default unit string is output.");

  Test_OK (1.0*""          , ""            , "Empty symbol"         );
  Test_OK (1.0*"rad"       , ""            , "Angle"                );
  Test_OK (1.0*"sr"        , ""            , "Solid angle"          );
  Test_OK (1.0*"mm"        , "m"           , "Distance"             );
  Test_OK (1.0*"Ms"        , "s"           , "Time"                 );
  Test_OK (1.0*"g"         , "kg"          , "Mass"                 );
  Test_OK (1.0*"dag"       , "mg"          , "Mass"                 );
  Test_OK (1.0*"mA"        , "A"           , "Current"              );
  Test_OK (1.0*"kK"        , "K"           , "Temperature"          );
  Test_OK (1.0*"mmol"      , "mol"         , "Substance"            );
  Test_OK (1.0*"ccd"       , "*cd"         , "Luminous_Intensity"   );
  Test_OK (1.0*"Hz"        , "/s"          , "Frequeny"             );
  Test_OK (1.0/"s"         , "s**(-1)"     , "Frequeny"             );
  Test_OK (1.0*"km/ks"     , "/(m**(-1)*s)", "Speed"                );
  Test_OK (1.0/"s*m**(-1)" , "km/h"        , "Speed"                );
  Test_OK (1.0*"m*kg/s**2" , "mN"          , "Force"                );
  Test_OK (1.0*"Pa"        , "N/m**2"      , "Pressure"             );
  Test_OK (1.0*"N*m"       , "J"           , "Energy"               );
  Test_OK (1.0*"eV"        , "aJ"          , "Energy"               );
  Test_OK (1.0*"W"         , "kJ/ms"       , "Watt"                 );
  Test_OK (1.0*"C"         , "s*A"         , "Charge"               );
  Test_OK (1.0*"W/A"       , "V"           , "Voltage"              );
  Test_OK (1.0*"uF"        , "C/V"         , "Capacitance"          );
  Test_OK (1.0*"V/A"       , "MOhm"        , "Resistance"           );
  Test_OK (1.0*"Ohm**(-1)" , "S"           , "Conductance"          );
  Test_OK (1.0/"Ohm"       , "mS"          , "Conductance"          );
  Test_OK (1.0*"V/s**(-1)" , "Wb"          , "Magnetic_Flux"        );
  Test_OK (1.0*"Wb/m**2"   , "T"           , "Magnetic_Flux_Density");
  Test_OK (1.0*"Wb*A**(-1)", "H"           , "Inductance"           );
  Test_OK (1.0*"cd*sr"     , "lm"          , "Luminous Flux"        );
  Test_OK (1.0*"lm/m**2"   , "lx"          , "Illuminance"          );
  Test_OK (1.0*"s**(-1/1)" , "Bq"          , "Activity"             );
  Test_OK (1.0/"Ms"        , "Bq"          , "Activity"             );
  Test_OK (1.0*"Gy"        , "J/g"         , "Absorbed Dose"        );
  Test_OK (1.0*"Sv"        , "J/Mg"        , "Dose Equivalent"      );
  Test_OK (1.0*"mol/s"     , "kat"         , "Catalytic_Activity"   );
  Test_OK (1.0*"min"       , "s"           , "minute"               );
  Test_OK (1.0*"h"         , "s"           , "hour"                 );
  Test_OK (1.0*"ha"        , "m**2"        , "Area"                 );
  Test_OK (1.0*"ml"        , "m**3"        , "liter (lower case)"   );
  Test_OK (1.0*"hL"        , "m**3"        , "liter (upper case)"   );

  Test_Step (Title => "Incorrect Symbols",
             Description => "Illegal_Unit must be raised.");

  Test_KO ("/m"      , "Wrong leading operator");
  Test_KO ("CM"      , "Wrong casing of ""cm""");
  Test_KO ("cm "     , "Trailing space");
  Test_KO (" Ms"     , "Leading space");
  Test_KO ("k g"     , "Space inside");
  Test_KO ("m-2"     , "Missing exponentiation");
  Test_KO ("s**-2"   , "Missing parenthesis");
  Test_KO ("m*"      , "Missing factor");
  Test_KO ("K**(4("  , "Wrong closing parenthesis");
  Test_KO ("s**(2+3)", "Illegal expression in exponent");
  Test_KO ("N**(2/3" , "Missing closing parenthesis");
  Test_KO ("Tdamol"  , "Unknown symbol");
  Test_KO ("da*m"    , "Prefix must lead without multiplication");
  Test_KO ("1*m"     , "No value allowed in symbol");
  Test_KO ("C/s*m"   , "No more factors after /");
  Test_KO ("S**(/3)" , "Missing numerator in exponent");
  Test_KO ("kg/(m/s)", "No / in denominator");
  Test_KO ("damin"   , "No prefix on min");
  Test_KO ("hh"      , "No prefix on h");
  Test_KO ("kha"     , "No prefix on ha");
  Test_KO ("Mkg"     , "No prefix on kg");
  Test_KO ("dad"     , "Prefix da on unknown symbol");
  Test_KO ("da"      , "deci-are not defined");
  Test_KO ("daa"     , "deka-are not defined");

  Test_Result;

end Test_SI_Units;
