------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2020, 2025, 2026 Christoph Karl Walter Grein
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

with Test_Rational_Arithmetics,
     Test_Rational_Image_Value,
     Test_Rational_Text_IO;
with Test_Predicates;
with Test_SI_Units;
with Test_SI_Math,
     Test_SI_Poly;
with Test_SI_Vectors;
with Test_SI_Celsius;
with Test_SI_Unformatted_IO;
with Test_SI_Text_IO_Strings,
     Test_SI_Text_IO_File,
     Test_SI_Text_IO_Put;

with Test_Support;
use  Test_Support;

procedure Run_All is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.2
  -- Date      21 March 2026
  --====================================================================
  -- Run all tests and display the global result.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  14.05.2020
  --  C.G.    1.1  01.07.2025 new Test_SI_Celsius
  --  C.G.    2.0  16.10.2025 'Image redefined
  --  C.G.    2.1  23.10.2025 Test_SI_Vectors new
  --  C.G.    2.2  21.03.2026 Test_Predicates new
  --====================================================================

begin

  Test_Rational_Arithmetics;
  Test_Rational_Image_Value;
  Test_Rational_Text_IO;

  Test_Predicates;

  Test_SI_Units;

  Test_SI_Math;
  Test_SI_Poly;
  Test_SI_Vectors;

  Test_SI_Celsius;

  Test_SI_Unformatted_IO;

  Test_SI_Text_IO_Strings;
  Test_SI_Text_IO_File;
  Test_SI_Text_IO_Put;

  Global_Result;

end Run_All;
