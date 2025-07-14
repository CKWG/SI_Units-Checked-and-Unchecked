------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2006, 2020 Christoph Karl Walter Grein
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
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
-- Author's homepage and email address:
--   http://home.T-Online.de/home/Christ-Usch.Grein/
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

with Ada.Exceptions;
use  Ada.Exceptions;

with Ada.Text_IO;
use  Ada.Text_IO;

with Test_Support;
use  Test_Support;

with Rational_Arithmetics.Text_IO;
use  Rational_Arithmetics, Rational_Arithmetics.Text_IO;

procedure Test_Rational_Text_IO is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      24 July 2020
  --====================================================================
  -- Test the text input output facility.
  -- The program reads the file Test_Rational_Text_IO.in and writes the
  -- result to standard output. Compare the output with that in file
  -- Test_Rational_Text_IO.out.expected - it must be identical.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  02.02.2006
  --  C.G.    2.0  24.07.2020 Compare expected and actual output
  --====================================================================

  Rat_in, Rat_out: File_Type;

  R: Rational;

  Line: String (1 .. 30);
  Last: Natural;

begin

  Test_Header (Title => "Test Rational Text_IO",
               Description => "Test reading from and writing to a file.");

  Test_Step (Title => "Produce Ouput",
             Description => "Read input file and write output file.");

  Open   (Rat_in , Name => "Test_Rational_Text_IO.in" , Mode => In_File);
  Create (Rat_out, Name => "Test_Rational_Text_IO.out", Mode => Out_File);

  while not End_Of_File (Rat_in) loop
    begin
      Get (Rat_in , R);
      Put (Rat_out, R);  New_Line (Rat_out);
    exception
      when E:others =>
        Get_Line (Rat_in , Line, Last);
        Put_Line (Rat_out, Exception_Name (E) & " => """ & Line (1 .. Last) & '"');
    end;
  end loop;

  Test_Step (Title => "Compare",
             Description => "Compare the actual with the expected output.");

  Close (Rat_in);
  Reset (Rat_out, In_File);
  Open  (Rat_in , In_File, "Test_Rational_Text_IO.out.expected");

  declare
    Line_i, Line_o: String (1 .. 55);
    Last_i, Last_o: Natural;
  begin
    loop
      Get_Line (Rat_in , Line_i, Last_i);
      Get_Line (Rat_out, Line_o, Last_o);
      Assert (Condition => Line_i (1 .. Last_i) = Line_o (1 .. Last_o),
              Message   => Line_o (1 .. Last_o),
              Only_Report_Error => False);
    end loop;
  exception
    When End_Error => null;
  end;

  Close  (Rat_in);
  Delete (Rat_out);

  Test_Result;

end Test_Rational_Text_IO;
