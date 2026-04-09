------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2018, 2020, 2025 Christoph Karl Walter Grein
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

with Ada.Assertions;
with Ada.Exceptions;
with Ada.Strings.Fixed;

with Ada.Text_IO;

with Test_Support;
use  Test_Support;

with SI.IO;
use  SI.IO, SI;

procedure Test_SI_Text_IO_Put is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   4.1
  -- Date      22 September 2025
  --====================================================================
  -- The Fore, Aft, Exp format modifiers apply unchanged to the numeric
  -- value only, so no test is neede.
  -- Test output for the dim modifier is written to the file
  -- Test_SI_Text_IO_Put.out, then compared with that in file
  -- Test_SI_Text_IO_Put.out.expected.
  -- Just a random sample suffices since the unit string test already
  -- warrants the correctness of the Dim parameter.
  -- Note: The test cannot be run with the unchecked version since
  --       dimension output is vital.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  06.08.2002
  --  C.G.    2.0  03.08.2018 Unit strings
  --  C.G.    2.1  29.09.2018 Unit syntax changed
  --  C.G.    3.0  14.05.2020 Dimensions generic parameter
  --  C.G.    3.1  03.07.2020 Split a test string into two
  --  C.G.    4.0  19.08.2025 New test: Put to file reimplemented
  --  C.G.    4.1  22.09.2025 [UA09-009 public] fixed  in alr 2.1.0
  --                          gnat_native=15.2.1; instead use new
  --                          function SI_is_Unchecked
  --====================================================================

  package A renames Ada.Text_IO;

  Physic, Result: A.File_Type;

  procedure Test_Case (X: Item; Dim: String := "") is
    use Ada.Exceptions, Ada.Strings.Fixed;
  begin
    Put (Result, X, Dim => Dim);  A.New_Line (Result);
  exception
    when Ex: others =>
      A.Put_Line (Result, '"' & Dim & '"' & (17 - Dim'Length) * ' ' & " => " & Exception_Name (Ex) & ": " & Exception_Message (Ex));
  end Test_Case;

begin

  Test_Header (Title => "Test SI Put to file",
               Description => "Test Dim formatting parameter.");

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

  A.Create (Result, A.Out_File, "Test_SI_Text_IO_Put.out");

  Test_Step (Title => "Modifiers that must fail",
             Description => "Only a few random samples.");

  A.Put_Line ("See file Test_SI_Text_IO_Put.out.expected");
  A.Put_Line (Result, "Must fail");  A.New_Line (Result);

  Test_Case (1.0 * ""    , Dim => "/");
  Test_Case (1.0 * "km/s", Dim => "km/s;");
  Test_Case (1.0 * "km/s", Dim => "*M/ms");
  Test_Case (1.0 * "km/s", Dim => "/(ms*m**(-1))s");
  Test_Case (1.0 * "s"   , Dim => "S");

  Test_Step (Title => "Modifiers that must pass",
             Description => "Only a few random samples.");

  A.Put_Line ("See file Test_SI_Text_IO_Put.out.expected");
  A.New_Line (Result);
  A.Put_Line (Result, "Must pass");  A.New_Line (Result);

  Test_Case (1.0 * "");
  Test_Case (1.0 * ""    , Dim => " ");         -- leading and trailing spaces
  Test_Case (1.0 * "km/s", Dim => "  km/s  ");  -- are ignored in Dim
  Test_Case (1.0 * "km/s");
  Test_Case (1.0 * "km/s", Dim => "*m/ms");
  Test_Case (1.0 * "km/s", Dim => "/(ms*dm**(-1))");

  Test_Case (2.33395*10.0**(-6)*"m**(-3)*kg**(-3/2)*s**(9/2)*A**(5/2)");  -- output lines must
  Test_Case (2.33395*10.0**(-6)*"m**(-3)*kg**(-3/2)*s**(9/2)*A**(5/2)",
             Dim =>"hs**(9/2)*dam**(-3)*mA**(5/2)*dag**(-3/2)");
  Test_Case (7.38060E-08*"dam**(-3)*hs**(9/2)*dag**(-3/2)*mA**(5/2)",     -- be identical
             Dim => "*m**(-3)*kg**(-3/2)*s**(9/2)*A**(5/2)");

  -----------------------------------------------------------

  Test_Step (Title => "Compare actual and expected output",
             Description => "Failing lines in actual result will be output.");

  A.Set_Output (A.Standard_Output);  -- else Reset raises Mode_Error
  A.Reset (Result, A.In_File);
  A.Open  (Physic, A.In_File , "Test_SI_Text_IO_Put.out.expected");

  declare
    Line_P, Line_R: String (1 .. 55);
    Last_P, Last_R: Natural;
  begin
    loop
      A.Get_Line (Physic, Line_P, Last_P);
      A.Get_Line (Result, Line_R, Last_R);
      Assert (Condition => Line_P (1 .. Last_P) = Line_R (1 .. Last_R),
              Message   => Line_R (1 .. Last_R),  -- actual result
              Only_Report_Error => True);
    end loop;
  exception
    When A.End_Error => null;
  end;

  A.Close  (Physic);
  A.Delete (Result);

  Test_Result;

end Test_SI_Text_IO_Put;
