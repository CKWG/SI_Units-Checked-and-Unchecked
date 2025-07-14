------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2008, 2018, 2020 Christoph Karl Walter Grein
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

with Ada.Assertions, Ada.IO_Exceptions;
use  Ada.Assertions, Ada.IO_Exceptions;

with SI.IO;
use  SI.IO, SI;

with Test_Support;
use  Test_Support;

procedure Test_SI_Text_IO_Strings is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   3.0
  -- Date      13 May 2020
  --====================================================================
  -- Test the Text_IO to and from string procedure.
  -- Note: Since output with default Dim parameter is tested, the test
  --       cannot run correctly on the unchecked version.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  28.04.2008
  --  C.G.    2.0  01.08.2018 Unit strings; test completely reworked
  --  C.G.    2.1  29.09.2018 Unit syntax changed
  --  C.G.    3.0  13.05.2020 Dimensions generic parameter
  --====================================================================

  procedure Test_Aft_Exp (Value: Item; Expected_D, Expected_F: String) is
    Result: String (2 .. 41);
    Got : Item;
    Last: Positive;
  begin
    Put (To => Result, X => Value);
    Assert (Condition => Result = Expected_D,  -- default
            Message   => '"' & Result & '"',
            Only_Report_Error => False);
    Get (From => Result, X => Got, Last => Last);
    Assert (Condition => Value = Got,
            Message   => "Get from default string",
            Only_Report_Error => False);
    Put (To => Result, X => Value, Aft => 2, Exp => 0);
    Assert (Condition => Result = Expected_F,  -- formatted
            Message   => '"' & Result & '"',
            Only_Report_Error => False);
    Get (From => Result, X => Got, Last => Last);
    Assert (Condition => Value = Got,
            Message   => "Get from formatted string",
            Only_Report_Error => False);
    New_Line;
  end Test_Aft_Exp;

  procedure Test_Prefix (Value: Item; Dim, Expected: String) is
    Result: String (2 .. 25);
    Got : Item;
    Last: Positive;
  begin
    Put (To => Result, X => Value, Aft => 2, Dim => Dim);
    Assert (Condition => Result = Expected,
            Message   => '"' & Result & '"',
            Only_Report_Error => False);
    Get (From => Result, X => Got, Last => Last);
    Assert (Condition => abs (Value - Got) <= 1.0E-6 * abs Value,
            Message   => "Get",
            Only_Report_Error => False);
  end Test_Prefix;

begin

  Test_Header (Title => "Test SI.Text_IO to and from strings",
               Description => ".");
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

  Test_Step (Title => "Test that the string is correctly filled",
             Description => "Aft and Ex modify Fore.");
  --                                     2345678901234567890123456789012345678901
  Test_Aft_Exp (One     , Expected_D => "                             1.00000E+00",
                          Expected_F => "                                    1.00");
  Test_Aft_Exp (10.0*"A", Expected_D => "                           1.00000E+01*A",
                          Expected_F => "                                 10.00*A");
  Test_Aft_Exp (20.0*"V", Expected_D => "     2.00000E+01*m**2*kg*s**(-3)*A**(-1)",
                          Expected_F => "           20.00*m**2*kg*s**(-3)*A**(-1)");

  ------------------------------------------------------------------------------
  Test_Step (Title => "Test prefixes",
             Description => "Prefix modifies exponent.");
  --                                                        234567890123456789012345"
  Test_Prefix (10_000.0*"m", "km"            , Expected => "             1.00E+01*km");
  Test_Prefix ( 1.0*"A"    , "mA"            , Expected => "             1.00E+03*mA");
  Test_Prefix ( 2.0/"s"    , "/Ms"           , Expected => "             2.00E+06/Ms");
  Test_Prefix ( 2.0/"s*Em" , "/(Ms*km**(+1))", Expected => "  2.00E-09/(Ms*km**(+1))");
  Test_Prefix (-3.0*"kV/mm", "V/m"           , Expected => "           -3.00E+06*V/m");

  ------------------------------------------------------------------------------
  Test_Step (Title => "Further Get tests",
             Description => ".");

  declare
    Value: Item;
    Last : Positive;
  begin
    Get ("1.0K", Value, Last);
    Assert (Condition => Value = One and Last = 3,
            Message   => "K unconsumed",
            Only_Report_Error => False);
    Get ("1.0*m k", Value, Last);
    Assert (Condition => Value = 1.0*"m" and Last = 5,
            Message   => "space stops reading",
            Only_Report_Error => False);
    Get ("1.0*S;", Value, Last);
    Assert (Condition => Value = 1.0/"Ohm" and Last = 5,
            Message   => "semicolon stops reading",
            Only_Report_Error => False);
  end;

  ------------------------------------------------------------------------------
  Test_Step (Title => "Test exceptions",
             Description => ".");

  declare
    Result: String (1 .. 5);
  begin
    Put (Result, 1.0*"A", Dim => "mA");
  exception
    when Layout_Error =>
      Assert (Condition => True,
              Message   => "Expected Layout_Error",
              Only_Report_Error => False);
  end;

  declare
    Result: String (1 .. 5);
  begin
    Put (Result, 1.0*"A", Dim => "m");
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Expected Unit_Error  m vs. A",
              Only_Report_Error => False);
  end;

  Test_Result;

end Test_SI_Text_IO_Strings;
