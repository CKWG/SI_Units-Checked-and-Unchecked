------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2003, 2018, 2020, 2025 Christoph Karl Walter Grein
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
with Ada.Exceptions, Ada.Text_IO;
use  Ada.Exceptions, Ada.Text_IO;

with Test_Support;
use  Test_Support;

with SI.IO;
use  SI.IO, SI;

procedure Test_SI_Text_IO_File is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   4.0
  -- Date      5 July 2025
  --====================================================================
  -- Test the IO package for a set of critical items by reading files
  -- Test_SI_Text_IO-10.in (Get with Width > 0) and Test_SI_Text_IO.in
  -- (Width = 0 and a lot of nonsensical units).
  -- Read the in files and compare the out files with the expected ones.
  -- Note: The test cannot be run with the unchecked version since
  --       dimension output is vital.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  25.07.2002
  --  C.G.    1.1  04.02.2003 Syntax for reading unit symbols changed
  --  C.G.    2.0  02.08.2018 Unit strings
  --  C.G.    3.0  14.05.2020 Dimensions generic parameter
  --  C.G.    4.0  05.07.2025 Add test for Width > 0
  --====================================================================

  Physic, Result: File_Type;

  I: Item;
  D: String (11 .. 30);
  L: Natural;

  procedure Compare is
    Line_P, Line_R: String (1 .. 30);
    Last_P, Last_R: Natural;
    P_End: Boolean := False;
  begin
    loop
      begin
        Get_Line (Physic, Line_P, Last_P);
      exception
        when End_Error =>
          P_End  := True;
      end;
      begin
        Get_Line (Result, Line_R, Last_R);
        if P_End then  -- R not yet at end
          Assert (Condition => False,
                  Message   => Line_R (1 .. Last_R) & " expected",
                  Only_Report_Error => False);
          Assert (Condition => False,
                  Message   => "Files have same length (Expected too short).",
                  Only_Report_Error => False);
          exit;
        end if;
      exception
        when End_Error =>  -- both at end?
          Assert (Condition => P_End,
                  Message   => "Files have same length" &
                  (if not P_End then " (Result too short))" else "."),
                  Only_Report_Error => False);
          exit;
      end;
      Assert (Condition => Line_P (1 .. Last_P) = Line_R (1 .. Last_R),
              Message   => Line_R (1 .. Last_R),
              Only_Report_Error => False);
    end loop;
  end Compare;

begin

  Test_Header (Title => "Test SI.Text_IO_File",
               Description => "Test reading from and writing to a file.");

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

  Test_Step (Title => "Test reading exactly Width characters",
             Description => "Only the first ten characters are read.");

  Open   (Physic, In_File , "Test_SI_Text_IO-10.in");
  Create (Result, Out_File, "Test_SI_Text_IO-10.out");

  Skip_Line (Physic);  -- just info about use

  while not End_Of_File (Physic) loop

    begin
      Get      (Physic, I, Width => 10);
      Get_Line (Physic, D, L);
      Put      (Result, I, Aft => 3, Exp => 0, Dim => D (16 .. L));
      New_Line (Result);
    exception
      when E: Data_Error | Illegal_Unit =>
        Get_Line (Physic, D, L);
        Put_Line (Result, Exception_Name (E));
    end;

  end loop;

  Close (Physic);
  Reset (Result, In_File);
  Open  (Physic, In_File , "Test_SI_Text_IO-10.out.expected");

  Compare;

  Close  (Physic);
  Delete (Result);

  -------------------------------------------------------

  Test_Step (Title => "Compare",
             Description => "Compare the actual with the expected output.");

  Open   (Physic, In_File , "Test_SI_Text_IO.in");
  Create (Result, Out_File, "Test_SI_Text_IO.out");

  while not End_Of_File (Physic) loop

    begin
      Get (Physic, I);
      Put (Result, I);  New_Line (Result);
    exception
      when E: Data_Error | Illegal_Unit =>
        declare
          Line: String (1 .. 20);
          Last: Natural;
        begin
          Get_Line (Physic, Line, Last);
          Put_Line (Result, Exception_Name (E) & " - rest of line: """ & Line (1 .. Last) & '"');
        end;
    end;

  end loop;

  Close (Physic);
  Reset (Result, In_File);
  Open  (Physic, In_File , "Test_SI_Text_IO.out.expected");

  Compare;

  Close  (Physic);
  Delete (Result);

  Test_Result;

end Test_SI_Text_IO_File;
