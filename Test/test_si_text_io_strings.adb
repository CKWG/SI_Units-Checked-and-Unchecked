------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2008, 2018, 2020, 2025 Christoph Karl Walter Grein
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
--   christ-usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Assertions, Ada.IO_Exceptions, Ada.Exceptions;
use  Ada.Assertions, Ada.IO_Exceptions, Ada.Exceptions;

with SI.IO;
use  SI.IO, SI;

with Test_Support;
use  Test_Support;

procedure Test_SI_Text_IO_Strings is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   4.1
  -- Date      22 September 2025
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
  --  C.G.    4.0  21.08.2025 Test adapted to new impl. of Gen_Text_IO
  --  C.G.    4.1  22.09.2025 [UA09-009 public] fixed  in alr 2.1.0
  --                          gnat_native=15.2.1; instead use new
  --                          function SI_is_Unchecked
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
    Assert (Condition => abs (Value - Got) <= 1.0E-3 * abs Value,
            Message   => "Get",
            Only_Report_Error => False);
  end Test_Prefix;

begin

  Test_Header (Title => "Test SI.Text_IO to and from strings",
               Description => ".");
  Test_Step (Title => "Assertions",
             Description => "Test that Assertion_Policy is Check.");

  Assert (Condition => not SI_is_Unchecked,
          Message   => "Assertion_Policy is Check",
          Only_Report_Error => False);

  if SI_is_Unchecked then
    Put_Line ("Switch on Assertion_Policy and use checked instantiation");
    Test_Result;
    return;
  end if;

  -----------------------------------------------------------

  Test_Step (Title => "Test that the string is correctly filled",
             Description => "Aft and Exp modify Fore.");
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
  Test_Prefix (10_000.0*"m"   , "km"            , Expected => "             1.00E+01*km");
  Test_Prefix ( 1.0*"A"       , "mA"            , Expected => "             1.00E+03*mA");
  Test_Prefix ( 2.0/"s**(1/3)", "/hs**(1/3)"    , Expected => "      9.28E+00/hs**(1/3)");
  Test_Prefix ( 2.0/"s*Em"    , "/(Ms*km**(+1))", Expected => "  2.00E-09/(Ms*km**(+1))");
  Test_Prefix (-3.0*"kV/mm"   , "V/m"           , Expected => "           -3.00E+06*V/m");

  ------------------------------------------------------------------------------
  Test_Step (Title => "Further Get tests",
             Description => "Charakters foreign to syntax stop reading; others raise Illegal_Unit.");

  declare
    Value: Item;
    Last : Positive;
  begin
    for C in Character when C in 'A' .. 'Z' | 'a' .. 'z' | '(' | '+' | '-' | ')' loop
      begin
        Get ("1.0" & C, Value, Last);
        Assert (Condition => False,
                Message   => C & "Illegal_Unit raised",
                Only_Report_Error => False);
      exception
        when Ex: Data_Error | Illegal_Unit =>
          Assert (Condition => True,
                  Message   => C & " consumed => " & Exception_Name (Ex) &
                  -- Exception message for Data_Error is compiler specific, so omit it.
                  (if Exception_Name (Ex) = "ADA.IO_EXCEPTIONS.DATA_ERROR" then ""
                   else ' ' & Exception_Message (Ex)),
                  Only_Report_Error => False);
      end;
    end loop;
  end;

  declare
    Value: Item;
    Last : Positive;
  begin --12345   rest not read
    Get ("1.0*m k", Value, Last);
    Assert (Condition => Value = 1.0*"m" and Last = 5,
            Message   => "space stops reading and is not read (Last =" & Last'Image & ')',
            Only_Report_Error => False);
    Get ("2.02*S;", Value, Last);
    Assert (Condition => Value = 2.02/"Ohm" and Last = 6,
            Message   => "semicolon stops reading and is not read (Last =" & Last'Image & ')',
            Only_Report_Error => False);
  end;

  ------------------------------------------------------------------------------
  Test_Step (Title => "Test exceptions",
             Description => "String too short: Layout_Error; wrong unit: Unit_Error.");

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
    Result: String (1 .. 5);  -- doesn't matter that string too short
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
