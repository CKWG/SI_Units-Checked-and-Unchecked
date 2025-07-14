------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2018, 2020 Christoph Karl Walter Grein
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

with Ada.Text_IO, Ada.Float_Text_IO;

with Test_Support;
use  Test_Support;

with SI.IO;
use  SI.IO, SI;
with Ada.Assertions;

procedure Test_SI_Text_IO_Put is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   3.1
  -- Date      3 July 2020
  --====================================================================
  -- Test the Fore, Exp, Dim format modifiers of Put.
  -- Compare the output in file Test_SI_Text_IO_Put.out with
  -- that in file Test_SI_Text_IO_Put.out.expected.
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
  --====================================================================

  package A renames Ada.Text_IO;

  Physic, Result: A.File_Type;

begin

  Test_Header (Title => "Test SI.Put",
               Description => "Test formatting parameters.");

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
              Only_Report_Error => True);
  end;

  -----------------------------------------------------------

  Test_Step (Title => "Compare",
             Description => "Compare the actual with the expected output.");

  A.Create (Result, A.Out_File, "Test_SI_Text_IO_Put.out");

  A.Set_Output (Result);

  Put (1.0 * "km/s");                          New_Line;
  Put (1.0 * "km/s", Dim => "*km/s");          New_Line;
  Put (1.0 * "km/s", Dim => "/(ms*m**(-1))");  New_Line;

  begin
    Put (1.0 * "km/s", Dim => "Km/s");
  exception
    when Illegal_Unit => Put_Line ("Illegal_Unit wrong casing Dim Km/s");
  end;

  begin
    Put (1.0 * "Km/s", Dim => "km/s");
  exception
    when Illegal_Unit => Put_Line ("Illegal_Unit wrong casing unit Km/s");
  end;

  begin
    Put (1.0 * "km/s", Dim => "km/s   A");
  exception
    when Illegal_Unit => Put_Line ("Illegal_Unit wrong format km/s   A");
  end;

  begin
    Put (1.0 * "s", Dim => "S");
  exception
    when Unit_Error => Put_Line ("Unit_Error wrong unit S (must be s)");
  end;

  -- Currently illegal, might be implemented some day
  -- Put (1.0 * "(km/s)**2", Dim => "km**2/s**2");  New_Line;
  -- Put (1.0 * "km/s"**2  , Dim => "(km/s)**2" );  New_Line;  Bad idea?

  New_Line;
  Put_Line ("Compare Ada.Text_IO.Float_Text_IO with SI.Text_IO;");
  Put_Line ("number formats must be identical:");
  New_Line;

  A.Put ('|');  Ada.Float_Text_IO.Put (1000.0, Fore => 0, Aft => 2, Exp => 0);  A.Put ('|');  A.Set_Col (15);  A.Put ('|');  Put (1.0 * "km/s", Fore => 0, Aft => 2, Exp => 0);  A.Put ('|');  New_Line;
  A.Put ('|');  Ada.Float_Text_IO.Put (1000.0, Fore => 0, Aft => 2, Exp => 1);  A.Put ('|');  A.Set_Col (15);  A.Put ('|');  Put (1.0 * "km/s", Fore => 0, Aft => 2, Exp => 1);  A.Put ('|');  New_Line;
  A.Put ('|');  Ada.Float_Text_IO.Put (1000.0, Fore => 0, Aft => 2, Exp => 3);  A.Put ('|');  A.Set_Col (15);  A.Put ('|');  Put (1.0 * "km/s", Fore => 0, Aft => 2, Exp => 3);  A.Put ('|');  New_Line;
  A.Put ('|');  Ada.Float_Text_IO.Put (1000.0, Fore => 3, Aft => 2, Exp => 0);  A.Put ('|');  A.Set_Col (15);  A.Put ('|');  Put (1.0 * "km/s", Fore => 3, Aft => 2, Exp => 0);  A.Put ('|');  New_Line;
  A.Put ('|');  Ada.Float_Text_IO.Put (1000.0, Fore => 3, Aft => 2, Exp => 1);  A.Put ('|');  A.Set_Col (15);  A.Put ('|');  Put (1.0 * "km/s", Fore => 3, Aft => 2, Exp => 1);  A.Put ('|');  New_Line;
  A.Put ('|');  Ada.Float_Text_IO.Put (1000.0, Fore => 3, Aft => 2, Exp => 3);  A.Put ('|');  A.Set_Col (15);  A.Put ('|');  Put (1.0 * "km/s", Fore => 3, Aft => 2, Exp => 3);  A.Put ('|');  New_Line;
  A.Put ('|');  Ada.Float_Text_IO.Put (1000.0, Fore => 5, Aft => 2, Exp => 0);  A.Put ('|');  A.Set_Col (15);  A.Put ('|');  Put (1.0 * "km/s", Fore => 5, Aft => 2, Exp => 0);  A.Put ('|');  New_Line;
  A.Put ('|');  Ada.Float_Text_IO.Put (1000.0, Fore => 5, Aft => 2, Exp => 1);  A.Put ('|');  A.Set_Col (15);  A.Put ('|');  Put (1.0 * "km/s", Fore => 5, Aft => 2, Exp => 1);  A.Put ('|');  New_Line;
  A.Put ('|');  Ada.Float_Text_IO.Put (1000.0, Fore => 5, Aft => 2, Exp => 3);  A.Put ('|');  A.Set_Col (15);  A.Put ('|');  Put (1.0 * "km/s", Fore => 5, Aft => 2, Exp => 3);  A.Put ('|');  New_Line;

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
              Message   => Line_R (1 .. Last_R),
              Only_Report_Error => False);
    end loop;
  exception
    When A.End_Error => null;
  end;

  A.Close  (Physic);
  A.Delete (Result);

  Test_Result;

end Test_SI_Text_IO_Put;
