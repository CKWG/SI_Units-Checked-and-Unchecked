------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2003, 2006, 2020 Christoph Karl Walter Grein
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

with Rational_Arithmetics;
use  Rational_Arithmetics;

with Test_Support;
use  Test_Support;

procedure Test_Rational_Arithmetics is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   3.2
  -- Date      3 July 2020
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  02.07.2002 with work-around for Gnat 3.14p bug
  --  C.G.    2.0  27.09.2002 Gnat work-around removed for 3.16w
  --                          (a few work-arounds are left)
  --  C.G.    3.0  26.02.2003 Last work-arounds removed for 3.16a
  --  C.G.    3.1  15.02.2006 Added Rational = Whole
  --  C.G.    3.2  03.07.2020 Take care of GNAT optimization level -O3
  --====================================================================

  R: Rational;

begin

  Test_Header (Title       => "Test Rational_Arithmetics",
               Description => "Test operations in the sequence of declaration.");

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test Constructors",
             Description => "");

  R := +5;
  Assert (Condition         => R = 5/1,
          Message           => "Constructors +5 = 5/1",
          Only_Report_Error => False);

  R := -2;
  Assert (Condition         => R = -2/1,  -- Note: -2/1 is -(2/1)
          Message           => "Constructors -2 = -2/1",
          Only_Report_Error => False);

  R := 2/2;
  Assert (Condition         => R = 1/1,
          Message           => "Reduce 2/2 = 1/1",
          Only_Report_Error => False);

  -- We need 0-10 in the denominator to get -10, because the unary
  -- "-" is not available.
  R := 5/(0-10);
  Assert (Condition         => R = -1/2,
          Message           => "5/(-10) = -1/2",
          Only_Report_Error => False);

  R := 0/(0-10);
  Assert (Condition         => R = 0/1,
          Message           => "0/(-10) = 0/1",
          Only_Report_Error => False);

  begin
    R := 7/0;
    Assert (Condition         => False,  -- this is a bug
            Message           => Numerator (R)'Image &    -- need this lest R might be optimized away
                                 "Constraint_Error 7/0",  -- (GNAT optimization level -O3) and test fails
            Only_Report_Error => False);
  exception
    when Constraint_Error =>
      Assert (Condition         => True,  -- this is expected
              Message           => "Constraint_Error 7/0",
              Only_Report_Error => False);
  end;

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test Reduction",
             Description => "");

  R := (3*37)/(5*37);
  Assert (Condition         => R = 3/5,
          Message           => "(3*37)/(5*37) = 3/5",
          Only_Report_Error => False);

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test Unary Operators",
             Description => "");

  R := +(7/5);
  Assert (Condition         => R = 7/5,
          Message           => "+(7/5)) = 7/5",
          Only_Report_Error => False);

  -- We need 0-2 in the numerator to get -2, because the unary
  -- "-" is not available.
  R := -(2/7);
  Assert (Condition         => R = (0-2)/7,
          Message           => "-(2/7) = (-2)/7",
          Only_Report_Error => False);

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test Binary Operators",
             Description => "");

  R := 2/3+1/3;
  Assert (Condition         => R = 1/1,
          Message           => "2/3+1/3 = 1/1",
          Only_Report_Error => False);

  R := 4/3-1/3;
  Assert (Condition         => R = 1/1,
          Message           => "4/3-1/3 = 1/1",
          Only_Report_Error => False);

  R := (4/3)*(6/2);
  Assert (Condition         => R = 4/1,
          Message           => "(4/3)*(6/2) = 4/1",
          Only_Report_Error => False);

  R := (4/3)/(-2/6);
  Assert (Condition         => R = -4/1,
          Message           => "(4/3)/(-2/6) = -4/1",
          Only_Report_Error => False);

  R := 7/100 - 7/100;
  Assert (Condition         => R = 0/1,
          Message           => "7/100 - 7/100 = 0/1",
          Only_Report_Error => False);

  begin
    R := (4/1)/(0/1);
    Assert (Condition         => False,  -- this is a bug
            Message           => Denominator (R)'Image & "Constraint_Error (4/1)/(0/1)",
            Only_Report_Error => False);
  exception
    when Constraint_Error =>
      Assert (Condition         => True,  -- this is expected
              Message           => "Constraint_Error (4/1)/(0/1)",
              Only_Report_Error => False);
  end;

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test Relational Operators",
             Description => "");

  R := 4/3;
  Assert (Condition         => R > 40/31,
          Message           => "4/3 > 40/31",
          Only_Report_Error => False);

  Assert (Condition         => not (R <= 40/31),
          Message           => "not 4/3 <= 40/31",
          Only_Report_Error => False);

  Assert (Condition         => R < 40/29,
          Message           => "4/3 < 40/29",
          Only_Report_Error => False);

  Assert (Condition         => not (R >= 40/29),
          Message           => "not 4/3 >= 40/29",
          Only_Report_Error => False);

  Assert (Condition         => Max (R, 40/29) = 40/29,
          Message           => "Max (4/3, 40/29) = 40/29",
          Only_Report_Error => False);

  Assert (Condition         => Min (R, 40/31) = 40/31,
          Message           => "Min (4/3, 40/31) = 40/31",
          Only_Report_Error => False);

  R := -21/19;
  Assert (Condition         => abs R = 21/19,
          Message           => "abs (-21/19) = 21/19",
          Only_Report_Error => False);

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test Mixed Operators",
             Description => "");

  R := 1+1/(4/8);
  Assert (Condition         => R = 3,
          Message           => "1+1/(4/8) = 3",
          Only_Report_Error => False);

  R := 1/(4/8)-1;
  Assert (Condition         => 1 = R,
          Message           => "1 = 1/(4/8)-1",
          Only_Report_Error => False);

  R := 2*(3/2)-(7/3)*3;
  Assert (Condition         => R = -4,
          Message           => "2*(3/2)-(7/3)*3 = -4",
          Only_Report_Error => False);

  R := (2/3)/2-8;
  Assert (Condition         => R = -(8-1/3),
          Message           => "(2/3)/2-8 = -(8-1/3)",
          Only_Report_Error => False);

  begin
    R := (7/1)/0;
    Assert (Condition         => False,  -- this is a bug
            Message           => Numerator (R)'Image & "Constraint_Error (7/1)/0",
            Only_Report_Error => False);
  exception
    when Constraint_Error =>
      Assert (Condition         => True,  -- this is expected
              Message           => "Constraint_Error (7/1)/0",
              Only_Report_Error => False);
  end;

  -----------------------------------------------------------------------
  Test_Result;

end Test_Rational_Arithmetics;
