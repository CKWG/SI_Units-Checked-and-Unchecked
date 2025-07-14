------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2006 Christoph Karl Walter Grein
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
--   http://www.christ-usch-grein.homepage.t-online.de/
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

with Rational_Arithmetics.Strings;
use  Rational_Arithmetics.Strings, Rational_Arithmetics;

with Test_Support;
use  Test_Support;

procedure Test_Rational_Image_Value is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.1
  -- Date      27 February 2006
  --====================================================================
  -- Test the Image and Value functions.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  15.02.2006
  --  C.G.    1.1  27.02.2006 Image and Value were moved into child
  --====================================================================

  procedure Value_Exc (X: in String) is
    R: Rational;  -- must not call Value here
  begin
    R := Value (X);                             -- must raise exception
    Assert (Condition         => False,         -- error if we arrive here
            Message           => '"' & X & '"' & " should raise exception",
            Only_Report_Error => False);
  exception
    when Constraint_Error =>                    -- expected
      Assert (Condition         => True,
              Message           => '"' & X & '"' & " raises exception",
              Only_Report_Error => False);
  end Value_Exc;

begin

  Test_Header (Title       => "Test Rational_Arithmetics",
               Description => "Test Image and Value functions.");

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test Image",
             Description => "");

  Assert (Condition         => Image (5/1) = " 5",
          Message           => "Image (5/1) =" & Image (5/1),
          Only_Report_Error => False);

  Assert (Condition         => Image (-14/2) = "-7",
          Message           => "Image (-14/2) = " & Image (-14/2),
          Only_Report_Error => False);

  Assert (Condition         => Image (3/4) = " 3/4",
          Message           => "Image (3/4) =" & Image (3/4),
          Only_Report_Error => False);

  Assert (Condition         => Image (-12/9) = "-4/3",
          Message           => "Image (-12/9) = " & Image (-12/9),
          Only_Report_Error => False);

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test Value",
             Description => "No exceptions");

  Assert (Condition         => Image (Value ("5")) = " 5",
          Message           => "Value (""5"")",
          Only_Report_Error => False);

  Assert (Condition         => Image (Value ("  5  ")) = " 5",
          Message           => "Value (""  5  "")",
          Only_Report_Error => False);

  Assert (Condition         => Image (Value ("  +8  ")) = " 8",
          Message           => "Value (""  +8  "")",
          Only_Report_Error => False);

  Assert (Condition         => Image (Value ("  -5  ")) = "-5",
          Message           => "Value (""  -5  "")",
          Only_Report_Error => False);

  Assert (Condition         => Image (Value ("  15/3  ")) = " 5",
          Message           => "Value (""  15/3  "")",
          Only_Report_Error => False);

  Assert (Condition         => Image (Value ("  +3/15  ")) = " 1/5",
          Message           => "Value (""  +3/15  "")",
          Only_Report_Error => False);

  Assert (Condition         => Image (Value ("  -15/3  ")) = "-5",
          Message           => "Value (""  -15/3  "")",
          Only_Report_Error => False);

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test Value",
             Description => "Exceptions");

  Value_Exc ("");            -- empty
  Value_Exc ("    ");        -- blank
  Value_Exc ("  -    ");     -- ?
  Value_Exc ("  /  ");       -- ?
  Value_Exc ("  5 /3 ");     -- blank before /
  Value_Exc ("  5/ 3  ");    -- blank after /
  Value_Exc ("  5/   ");     -- denominator missing
  Value_Exc ("  -5/3 1  ");  -- something follows denominator
  Value_Exc ("  + 5  ");     -- blank after +
  Value_Exc ("  - 5  ");     -- blank after -
  Value_Exc ("  + 15/3  ");  -- blank after +
  Value_Exc ("  - 15/3  ");  -- blank after -

  -----------------------------------------------------------------------
  Test_Result;

end Test_Rational_Image_Value;
