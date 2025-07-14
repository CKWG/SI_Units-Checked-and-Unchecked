------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2025 Christoph Karl Walter Grein
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
with Ada.Text_IO;

with Test_Support;
use  Test_Support;

with SI.Temp.IO, SI.Temp.Strings;
use  SI.Temp.IO, SI.Temp.Strings, SI.Temp, SI;

procedure Test_SI_Celsius is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      3 July 2025
  --====================================================================
  -- Test SI.Temperature and children, i.e. the Celsius operations.
  -- The program reads the file Test_SI_Celsius_IO.in and writes the
  -- result to Test_SI_Celsius_IO.out.
  -- Compare it with that in file Test_SI_Celsius_IO.out.expected - it
  -- must be identical.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  03.07.2025
  --====================================================================

  Physic, Result: Ada.Text_IO.File_Type;

begin

  Test_Header (Title => "Test SI.Celsius",
               Description => "Test the Celsius operations and IO.");

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

  Test_Step (Title => "Test Constructor",
             Description => "Corret and incorrect use.");

  declare
    T: Celsius := 0.0*"°C";
  begin
    Assert (Condition =>  to_Kelvin (T) = 273.15*"K",
            Message   => "Correct Celsius zero point",
            Only_Report_Error => False);
    Assert (Condition => to_Celsius (to_Kelvin (T)) = T,
            Message   => "Correct conversion between Celsius and Kelvin",
            Only_Report_Error => False);
  end;

  declare
    T: Celsius;
  begin
    T := 100.0*"°C ";
    Assert (Condition => False,
            Message   => "Wrong unit indication recognized 1",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Wrong unit indication recognized 1",
              Only_Report_Error => False);
  end;

  declare
    T: Celsius;
  begin
    T := 100.0*"°ökjdC";
    Assert (Condition => False,
            Message   => "Wrong unit indication recognized 2",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Wrong unit indication recognized 2",
              Only_Report_Error => False);
  end;

  -----------------------------------------------------------

  Test_Step (Title => "Test Image and Value",
             Description => "Compare the result of Image with expectation." &
                            " Call Value on a modification of result.");

  declare
    R: constant := 0.1;
    T: constant Celsius := R*"°C";
    I: constant String := Image (T);
  begin
    Assert (Condition => I = Float'Image (R) & "°C",
            Message   => "Correct Image " & I,
            Only_Report_Error => False);
    Assert (Condition => Value (I) = T,
            Message   => "Correct Value",
            Only_Report_Error => False);
    Assert (Condition => Value ("   " & I & "   ") = T,
            Message   => "Value ignores leading and trailing blanks",
            Only_Report_Error => False);
  end;

  declare
    T: Celsius;
  begin
    T := Value ("   13#4.a12#E-3°C   ");
    Assert (Condition => True,
            Message   => "Value accepts based literal",
            Only_Report_Error => False);
    Put_Line (Image (T));
  end;

  -----------------------------------------------------------

  Test_Step (Title => "Test Value on wrong strings",
             Description => "Check that exceptions are raised.");

  begin
    Assert (Condition => Value ("   4.12°xC   ") = 4.12*"°C" and False,
            Message   => "Value raises exception with wrong unit",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
            Message   => "Value raises exception with wrong unit",
            Only_Report_Error => False);
  end;

  begin
    Assert (Condition => Value ("   4.12°C  x ") = 4.12*"°C" and False,
            Message   => "Value raises exception with trailing nonblank",
            Only_Report_Error => False);
  exception
    when Unit_Error =>
      Assert (Condition => True,
            Message   => "Value raises exception with trailing nonblank",
            Only_Report_Error => False);
  end;

  -----------------------------------------------------------

  Test_Step (Title => "Compare",
             Description => "Compare the actual with the expected output.");

  declare

    use Ada.Text_IO;

  begin

    Open   (Physic, In_File , "Test_SI_Celsius_IO.in");
    Create (Result, Out_File, "Test_SI_Celsius_IO.out");

    loop

      declare
        T: Celsius;
      begin
        Get (Physic, T);
        Put (Result, T, Aft => 1, Exp => 0);  New_Line (Result);
      exception
        when Unit_Error => Put_Line (Result, "Unit_Error");
        when Data_Error => Put_Line (Result, "Data_Error");  Skip_Line (Physic);
        when End_Error  => exit;
      end;

    end loop;

    Close (Physic);
    Open  (Physic, In_File, "Test_SI_Celsius_IO.out.expected");
    Reset (Result, In_File);

    loop

      declare
        Line_E, Line_R: String (1 .. 20);
        Last_E, Last_R: Natural;
      begin
        Get_Line (Physic, Line_E, Last_E);
        Get_Line (Result, Line_R, Last_R);
        Assert (Condition => Line_E (1 .. Last_E) = Line_R (1 .. Last_R),
                Message   => Line_R (1 .. Last_R) & " => Lines are equal",
                Only_Report_Error => False);
      exception
        when End_Error => exit;
      end;

    end loop;

    Close  (Physic);
    Delete (Result);

  end;

  -----------------------------------------------------------

  Test_Step (Title => "Test Put and Get for String",
             Description => "Read with Get what Put wrote.");

  declare

    S: String (1 .. 30) := (7 | 25 => '|', others => ' ');
    L: Natural;
    C: Celsius;

  begin

    Put_Line ('"' & S & '"');

    Put (S (7 .. 25), 1815.0*"°C");
    Put_Line ('"' & S & '"');
    Get (S, C, L);
    Assert (Condition => L = 25,
            Message   => "Last as expected",
            Only_Report_Error => False);
    Assert (Condition => C = 1815.0*"°C",
            Message   => "Read what Written",
            Only_Report_Error => False);

    S (27) := 'X';
    Put (S (7 .. 25), 1983.0*"°C");
    Put_Line ('"' & S & '"');
    Get (S, C, L);
    Assert (Condition => L = 25,
            Message   => "Spurious character not read",
            Only_Report_Error => False);
    Assert (Condition => C = 1983.0*"°C",
            Message   => "Read new value",
            Only_Report_Error => False);

  end;

  -----------------------------------------------------------

  Test_Step (Title => "Test failing Get for String",
             Description => "Spurious characters make Get fail.");

  declare

    S: String (1 .. 30) := (7 | 25 => '|', 26 .. 27 => 'X', others => ' ');
    L: Natural;
    C: Celsius;

  begin

    Put_Line ('"' & S & '"');

    Put (S (7 .. 25), 1995.0*"°C", Exp => 0);
    Put_Line ('"' & S & '"');
    Get (S, C, L);

  exception
    when Unit_Error =>
      Assert (Condition => True,
              Message   => "Unit_Error raised",
              Only_Report_Error => False);
      Put_Line ("Last" & L'Image & " is invalid.");

  end;

  Test_Result;

end Test_SI_Celsius;
