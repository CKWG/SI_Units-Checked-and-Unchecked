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

with Ada.Assertions, Ada.Exceptions;
use  Ada.Assertions, Ada.Exceptions;

with SI;
use  SI;

with Test_Support;
use  Test_Support;

procedure Test_Predicates is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      21 March 2026
  --====================================================================
  -- This test shows that even subtypes with identical dimension can
  -- raise Unit_Error with different subtype-specific messages.
  -- Note: The test cannot be run with the unchecked version since
  --       presence of dimension indication is vital.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  21.03.2026
  --====================================================================

  ------------------------------------------------------------------------------
  -- The objective of this introductory test is to show that Dynamic_Predicate
  -- syntax is obeyed in general. The idea (or prejudice) is - it works with
  -- scalar types.
  -- With the history of the SI library and its plethora of GNAT bugs corrected
  -- over the time, the doubt that it will work with more complex tpyes like
  -- SI.Item is obvious.
  -- Contrary to the expectations above:
  --  * GNAT CE2021 passes Step 2, fails  Step 3; passes Step 4.
  --  * GNAT 15.2.1 fails  Step 2, passes Step 3; passes Step 4.
  -- Since Step 2 does not affect the SI library, the bug has not been reported
  -- to Bugzilla.

  type Integer is range 0 .. 100;
  Not_Even: exception;
  subtype Even is Integer with Dynamic_Predicate => Even mod 2 = 0,
                               Predicate_Failure => raise Not_Even with "Even subtype";

  procedure Test_Even_Exception is
  begin
    declare
      X: Even := 2;
    begin
      X := 1;
    exception
      when E: others =>
        Put_Line (Exception_Name (E) & " => " & Exception_Message (E));
        Assert (Condition => Exception_Name (E) = "TEST_PREDICATE.NOT_EVEN",
                Message   => "Exception as expected in assignment",
                Only_Report_Error => False);
        New_Line;
    end;
    declare
      X: Even := 1;
    begin
      null;
    end;
  exception
    when E: others =>
      Put_Line (Exception_Name (E) & " => " & Exception_Message (E));
      Assert (Condition => Exception_Name (E) = "TEST_PREDICATE.NOT_EVEN",
              Message   => "Exception as expected in initialization",
              Only_Report_Error => False);
  end Test_Even_Exception;

  ------------------------------------------------------------------------------
  -- The objective of this test is to show that assignments with compatible
  -- units do not fail, however those with incompatible ones emit the correct
  -- message.
  -- Affected units:
  --   Dimensionless, Angle, Solid_Angle;
  --   Frequency, Angular_Frequency, Radio_Activity;
  --   Torque, Energy;
  --   Luminous_Intensity, Luminous_Flux;
  --   Absorbed_Dose, Dose_Equivalent.

  procedure Test_Item_Exception is
  begin
    declare
      X: Dimensionless := 0.0*"";
    begin
      X := 1.0*"rad";
      Assert (Condition => True,
              Message   => "Dimensionless: Assignment with compatible unit rad",
              Only_Report_Error => False);
      X := 1.0*"m";
      Assert (Condition => False,
              Message   => "Dimensionless: Assignment with wrong unit m");
      New_Line;
    exception
      when E: others =>
        Put_Line (Exception_Name (E) & " => " & Exception_Message (E));
        Assert (Condition => Exception_Name (E) = "SI.UNIT_ERROR",
                Message   => "Exception as expected in assignment",
                Only_Report_Error => False);
        Assert (Condition => Exception_Message (E) = "Dimensionless",
                Message   => "Message as expected in assignment",
                Only_Report_Error => False);
        New_Line;
    end;
    declare
      X: Angle := 0.0*"rad";
    begin
      X := 1.0*"m";
      Assert (Condition => False,
              Message   => "Angle: Assignment with wrong unit m");
      New_Line;
    exception
      when E: others =>
        Put_Line (Exception_Name (E) & " => " & Exception_Message (E));
        Assert (Condition => Exception_Name (E) = "SI.UNIT_ERROR",
                Message   => "Exception as expected in assignment",
                Only_Report_Error => False);
        Assert (Condition => Exception_Message (E) = "Angle",
                Message   => "Message as expected in assignment",
                Only_Report_Error => False);
        New_Line;
    end;
    declare
      X: Area := 0.0*"m**3";
    begin
      null;
    end;
  exception
    when E: others =>
      Put_Line (Exception_Name (E) & " => " & Exception_Message (E));
      Assert (Condition => Exception_Name (E) = "SI.UNIT_ERROR",
              Message   => "Exception as expected in initialization",
              Only_Report_Error => False);
        Assert (Condition => Exception_Message (E) = "Area",
                Message   => "Message as expected in initialization",
                Only_Report_Error => False);
  end Test_Item_Exception;

  ------------------------------------------------------------------------------
  -- The objective of this test is to show that objects of predicate-free
  -- first subtype Item can change the unit, but still raise an exception
  -- when combined with incompatible units.

  procedure Test_Incompatibility is
    X: Item := 2.0*"m/s";
  begin
    X := 4.0*"N";
    Put_Line ("Unit change possible for base type.");
    X := X + 1.0*"m";
  exception
    when E: others =>
      Put_Line (Exception_Name (E) & " => " & Exception_Message (E));
      Assert (Condition => Exception_Name (E) = "SI.UNIT_ERROR",
              Message   => "Exception as expected",
              Only_Report_Error => False);
  end Test_Incompatibility;

begin  --=======================================================================

  Test_Header (Title => "Test Dynamic_Predicate",
               Description => "Test that the Dynamic_Predicate is properly executed.");

  ------------------------------------------------------------------------------
  -- If this introductora test fails, the reason might be that Assertion_Error
  -- is raised instead. => Change the body of SI_is_Unchecked.
  -- If this isn't the reason, the compiler has a fundamental problem.

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

  ------------------------------------------------------------------------------

  Test_Step (Title => "Test Code for Integer",
             Description => "Directly assign incompatible subtypes.");

  Test_Even_Exception;

  ------------------------------------------------------------------------------

  Test_Step (Title => "Test Code for Item",
             Description => "Directly assign incompatible subtypes.");

  Test_Item_Exception;

  -----------------------------------------------------------

  Test_Step (Title => "Test predicate-free Object",
             Description => "Combine incompatible units.");

  Test_Incompatibility;

  -----------------------------------------------------------

  Test_Result;

end Test_Predicates;














