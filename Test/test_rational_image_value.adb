------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2006, 2025 Christoph Karl Walter Grein
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
-- Author's email address:
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

with Rational_Arithmetics;
use  Rational_Arithmetics;

with Test_Support;
use  Test_Support;

procedure Test_Rational_Image_Value is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      16 October 2025
  --====================================================================
  -- Test the Image and Value functions.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  15.02.2006
  --  C.G.    1.1  27.02.2006 Image and Value were moved into child
  --  C.G.    2.0  16.10.2025 'Image redefined
  --====================================================================

  procedure Valid_Image (X: Rational; Fraction, Expected: String) is
  begin
    Assert (Condition         => X'Image = Expected,
            Message           => "Image of " & Fraction & " =" & X'Image,
            Only_Report_Error => False);
  end Valid_Image;

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
             Description => "No exceptions");

  Valid_Image (  5/1, Fraction => "  5/1", Expected => " 5");
  Valid_Image (-14/2, Fraction => "-14/2", Expected => "-7");
  Valid_Image (  3/4, Fraction => "  3/4", Expected => " 3/4");
  Valid_Image (-12/9, Fraction => "-12/9", Expected => "-4/3");

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test Value",
             Description => "No exceptions");

  Assert (Condition         => Value ("5")'Image = " 5",
          Message           => "Value (""5"")",
          Only_Report_Error => False);

  Assert (Condition         => Value ("  5  ")'Image = " 5",
          Message           => "Value (""  5  "")",
          Only_Report_Error => False);

  Assert (Condition         => Value ("  +8  ")'Image = " 8",
          Message           => "Value (""  +8  "")",
          Only_Report_Error => False);

  Assert (Condition         => Value ("  -5  ")'Image = "-5",
          Message           => "Value (""  -5  "")",
          Only_Report_Error => False);

  Assert (Condition         => Value ("  15/3  ")'Image = " 5",
          Message           => "Value (""  15/3  "")",
          Only_Report_Error => False);

  Assert (Condition         => Value ("  +3/15  ")'Image = " 1/5",
          Message           => "Value (""  +3/15  "")",
          Only_Report_Error => False);

  Assert (Condition         => Value ("  -15/3  ")'Image = "-5",
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
