------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2018, 2019, 2020 Christoph Karl Walter Grein
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
--   christ-usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Assertions,
     Ada.Strings.Fixed;
with Ada.Strings.Maps;

with Test_Support;
use  Test_Support;

with SI.Strings;
use  SI.Strings, SI;

procedure Test_SI_Strings is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      13 May 2020
  --====================================================================
  -- Test the Image and Value functions.
  -- (Only a few simple tests. More extensive tests are performed on
  -- Text_IO.)
  -- Note: Since reversity of Image and Value is tested, the test cannot
  --       run correctly on the unchecked version.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  31.07.2018
  --  C.G.    1.1  28.09.2018 Unit syntax changed
  --  C.G.    1.2  13.02.2019 Test slightly improved
  --  C.G.    2.0  13.05.2020 Dimensions generic parameter
  --====================================================================

  procedure Test_Image_Value (Legal_Image: String; Expected: Item) is
  begin
    -- Comparing images rather than values avoids small rounding errors:
    Assert (Condition => Image (Value (Legal_Image)) = Image (Expected),
            Message   => "Value of """ & Legal_Image & """ => """ & Image (Expected) & '"',
            Only_Report_Error => False);
  exception
    when Illegal_Unit =>
      Assert (Condition => False,
              Message   => '"' & Legal_Image & """ raised Illegal_Unit",
              Only_Report_Error => False);
  end Test_Image_Value;

  procedure Test_KO (Illegal_Image: String; Text: String) is
    X: Item;
  begin
    X := Value (Illegal_Image);
    Assert (Condition => False,
            Message   => '"' & Illegal_Image & """ should have raised exception",
            Only_Report_Error => False);
  exception
    when Illegal_Unit =>
      Assert (Condition => True,
              Message   => '"' & Illegal_Image & """: " & Text & " Illegal_Unit",
              Only_Report_Error => False);
  end Test_KO;

begin

  Test_Header (Title => "Test SI.Strings",
               Description => "Test the Image and Value functions.");

  Test_Step (Title => "Assertions",
             Description => "Test that Assertion_Policy is Check.");

  declare
    T: Time;
  begin
    T := 1.0*"S";
    Assert (Condition => False,
            Message   => "Switch on Assertion_Policy and use checked instantiation",
            Only_Report_Error => False);
    Test_Result;
    return;
  exception
    when Ada.Assertions.Assertion_Error =>
      Assert (Condition => True,
              Message   => "Exception Assertion_Error as expected",
              Only_Report_Error => False);
  end;

  -----------------------------------------------------------

  Test_Step (Title => "Image and Value",
             Description => "Functions must be inverses of one another.");

  Assert (Condition => Image (Value (Image (5.0*"pF"))) = " 5.00000E-12*m**(-2)*kg**(-1)*s**4*A**2",
          Message   => "Image of Value of Image """ & Image (5.0*"pF") & '"',
          Only_Report_Error => False);

  Test_Image_Value ("  1.0"         ,   One);
  Test_Image_Value ("  1.0"         ,   1.0   *""    );
  Test_Image_Value ("  10*K "       ,  10.0   *"K"   );    -- no decimal dot; shouldn't this be "10.0*K"? RM 3.5(55/3, 39.6/2, 39.7/2), 2.4(1)
  Test_Image_Value ("  5.0*daOhm  " ,   5.0E+1*"Ohm" );  -- daOhm is longest legal symbol; leading and trailing spaces are ignored
  Test_Image_Value ("-42.0*Mg"      , -42.0E+3*"kg"  );
  Test_Image_Value (" -2.0/ms"      ,  -2.0E+3*"Hz"  );
  Test_Image_Value ("-02.0/us  "    ,  -2.0E+3*"kBq" );
  Test_Image_Value ("  1.0/(m)"     ,   1.0   / "m"  );
  Test_Image_Value ("  1.0/(s*m)"   ,   1.0   / "m*s");
  Test_Image_Value (" 18.1*W/(m*K)" ,  18.1E+0*"kg*m*s**(-3)*K**(-1)");
  Test_Image_Value ("  1.0*V**(3/2)",   1.0E+0*"m**3*kg**(3/2)*s**(-9/2)*A**(-3/2)");  -- appears in Schottky-Langmuir equation

  -----------------------------------------------------------

  Test_Step (Title => "Illegal Images",
             Description => ".");

  declare
    X: Item;
  begin
    X := Value ("-10.0K");
    Assert (Condition => False,
            Message   => """-10.0K"" should have raised exception",
            Only_Report_Error => False);
  exception
    when Constraint_Error =>
      Assert (Condition => True,
              Message   => """-10.0K"" - Ada's Value raises Constraint_Error, RM 3.5(39.12/3)",
              Only_Report_Error => False);
  end;

  Test_KO ("10*K ."     , "only trailing spaces allowed");
  Test_KO ("1.0*dakatxx", "symbol too long");
  Test_KO ("1.0*mOhm/"  , "trailing /");
  Test_KO ("1.0*mmin"   , "no prefix allowed");
  Test_KO ("1.0*mh"     , "no refix allowed");
  Test_KO ("1.0*Mkg"    , "must be Mg");
  Test_KO ("1.0*W/(m"   , "missing )");
  Test_KO ("1.0*W)"     , "missing divisor");
  Test_KO ("1.0*W*(m)"  , "no parentheses in product");
  Test_KO ("1.0*W/(m*K" , "missing )");
  Test_KO ("1.0*W/(m*K!", "missing )");
  Test_KO ("1.0*W/m/K"  , "must be W/(m*K)");
  Test_KO ("1.0*W/m*K"  , "no mix of * and /");
  Test_KO ("1.0*/m*K"   , "missing parentheses");
  Test_KO ("1.0*W/(m*K" , "missing )");
  Test_KO ("1.0*W/(m)*K", "no unit allowed after )");
  Test_KO ("1.0*W/(m/K)", "no second / allowed");
  Test_KO ("1.0/(m*K)A" , "only trailing spaces allowed");
  Test_KO ("1.0*(m)"    , "no parentheses in product");
  Test_KO ("1.0 *S"     , "no space after number");
  Test_KO ("1.0* s"     , "no space after operator");

  Test_Result;

end Test_SI_Strings;
