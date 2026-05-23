------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2026 Christoph Karl Walter Grein
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

with Ada.Exceptions, Ada.Text_IO;
use  Ada.Exceptions, Ada.Text_IO;

with Test_Support;
use  Test_Support;

with SI.IO;
use  SI.IO, SI;

procedure Test_SI_Text_IO_Table is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      8 May 2026
  --====================================================================
  -- Test the IO package for Get with Width > 0.
  -- Read the in files and compare the out files with the expected ones.
  -- Note: The test cannot be run with the unchecked version since
  --       dimension output is vital.
  -- There is a GNAT bug still in the latest edition (don't know the
  -- version number), see (*) in Table_KO below: It accepts trainling
  -- blanks. AI22-0151-1 takes care of this.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  08.05.2026
  --====================================================================

  type Table_Entry is record
    Length: Positive;          -- only so many colums in table
    Data  : String (1 .. 32);  -- characters not to be consumed filled with '#'
  end record;

  type Table is array (Positive range <>) of Table_Entry;

  Table_OK: constant Table :=
    --          1234567890123---1234567890123456
    (1 => (32, "         1815                   "),   -- dimensionless
     2 => (32, "        1983.   km              "),   -- left-aligned
     3 => (32, "       1983.1               km/h"),   -- right-aligned
     4 => (32, "     .1983004        kat/l      "),   -- unaligned
     5 => (21, "    1983.22E1   /umol###########"));  -- EoL in unit
    --          12345678901234567890123456789012
  Expect_OK: constant Table :=
    (1 => (12, "  1.81500E+3                    "),
     2 => (14, "  1.98300E+6*m                  "),
     3 => (22, "  5.50861E+2*m*s**(-1)          "),
     4 => (32, "  1.98300E+2*m**(-3)*s**(-1)*mol"),
     5 => (23, "  1.98322E+10*mol**(-1)         "));

  Table_KO: constant Table :=
    --          1234567890123---1234567890123456
    (1 => (32, "          1995                  "),   -- padding not empty,
     2 => (32, "    1995e-1                     "),   -- value not right-aligned (*)
     3 => (32, "    -1995E-27   m/ s            "),   -- illegal unit
     4 => (10, "   2005e-1######################"),   -- EoL in value
     5 => (13, "      2005E+0###################"),   -- EoL after value
     6 => (15, "      2012.09  #################"));  -- EoL in padding
    --          12345678901234567890123456789012
  Expect_KO: constant Table :=
    (1 => (31, "Data_Error => padding incorrect "),
     2 => (22, "Data_Error => in value          "),  -- (*)
     3 => (31, "Illegal_Unit => divisor missing "),
     4 => (22, "Data_Error => in value          "),
     5 => (22, "Data_Error => in value          "),
     6 => (31, "Data_Error => padding incorrect "));

  Physic, Result: File_Type;

  I: Item;

  procedure Compare (Expectation: in Table) is
    Line_R: String (1 .. 33);
    Last_R: Natural;
    procedure Put_Line (Item: String) renames Test_Support.Put_Line;
  begin
    for Line_E of Expectation loop
      Get_Line (Result, Line_R, Last_R);
      Assert (Condition => Line_E.Data (1 .. Line_E.Length) = Line_R (1 .. Last_R),
              Message   => Line_R (1 .. Last_R),
              Only_Report_Error => False);
    end loop;
  end Compare;

begin

  Test_Header (Title => "Test SI.Text_IO_Table",
               Description => "Test reading from a table.");

  Test_Step (Title => "Assertions",
             Description => "Test that Assertion_Policy is Check.");

  Assert (Condition => not SI_is_Unchecked,
          Message   => "Assertion_Policy is Check",
          Only_Report_Error => False);

  if SI_is_Unchecked then
    Test_Support.Put_Line ("Switch on Assertion_Policy and use checked instantiation");
    Test_Result;
    return;
  end if;

  -----------------------------------------------------------

  Test_Step (Title => "Test reading a correct table",
             Description => "Units aligned and unaligned are accepted.");

  Create (Physic, Out_File, "Table-in.txt");

  for Line of Table_OK loop
    Put_Line (Physic, Line.Data (1 .. Line.Length));
  end loop;

  Reset  (Physic, In_File);
  Create (Result, Out_File, "Table-out.txt");

  while not End_Of_File (Physic) loop
    Get (Physic, I, Width => 13, Pad => 3, Unit => 16);  Skip_Line (Physic);
    Put (Result, I, Fore  =>  3, Aft => 5, Exp  =>  1);  New_Line  (Result);
  end loop;

  -----------------------------------------------------------

  Test_Step (Title => "Compare OK result with expectation",
             Description => "");

  Reset (Result, In_File);

  Compare (Expect_OK);

  -----------------------------------------------------------

  Test_Step (Title => "Test reading an incorrect table",
             Description => "Check that correct exception is raised.");

  Reset (Physic, Out_File);

  for Line of Table_KO loop
    Put_Line (Physic, Line.Data (1 .. Line.Length));
  end loop;

  Reset (Physic, In_File );
  Reset (Result, Out_File);

  while not End_Of_File (Physic) loop
    begin
      Get (Physic, I, Width => 13, Pad => 3, Unit => 16);  Skip_Line (Physic);
      Put (Result, I, Fore  =>  3, Aft => 5, Exp  =>  1);  New_Line  (Result);
    exception
      when E: Data_Error =>
        Skip_Line (Physic);
        Put_Line  (Result, "Data_Error => " & Exception_Message (E));
      when I: Illegal_Unit =>
        Skip_Line (Physic);
        Put_Line  (Result, "Illegal_Unit => " & Exception_Message (I));
      end;
  end loop;

  -----------------------------------------------------------

  Test_Step (Title => "Compare KO result with expectation",
             Description => "");

  Delete (Physic);
  Reset  (Result, In_File);

  Compare (Expect_KO);

  Delete (Result);

  Test_Result;

end Test_SI_Text_IO_Table;
