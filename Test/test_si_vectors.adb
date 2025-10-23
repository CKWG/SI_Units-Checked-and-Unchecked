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

with Elementary_Functions;
use  Elementary_Functions;

with SI.VS;
use  SI.VS, SI;

with Test_Support;
use  Test_Support;

procedure Test_SI_Vectors is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      23 October 2025
  --====================================================================
  -- Test D = V * T, D and V vectors.
  -- More tests in directory Examples.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  23.10.2025
  --====================================================================

  N: constant Vector := Normalize ((1.0, 1.0, 1.0));

  D: constant Position :=  N * (1.0*"km");
  V: constant Velocity := -N * (1.0*"dam/min");

begin

  Test_Header (Title       => "Test Vectorspace",
               Description => "Linear motion with constant velocity.");

  -----------------------------------------------------------------------
  Test_Step (Title       => "Test vector movement",
             Description => "Compute time to reach home.");

  Put_Line (Position'(D + V * (abs D / abs V))'Image);  -- less accurate than scalar
  Put_Line ("Distance to origin:" & Length'(abs (D + V * (abs D / abs V) - 1.0*"µm" * Null_Vector))'Image);
  Assert (Condition => abs (D + V * (abs D / abs V) - 1.0*"µm" * Null_Vector) <= 0.11*"mm",
          Message   => "(Vector) Time to reach origin:" & Time'(abs D / abs V)'Image,
          Only_Report_Error => False);

  Put_Line (Length'(abs D - abs V * (abs D / abs V))'Image);
  Assert (Condition => abs D - abs V * (abs D / abs V) = 0.0*"dam",
          Message   => "(Salar) Time to reach origin:" & Time'(abs D / abs V)'Image,
          Only_Report_Error => False);

  -----------------------------------------------------------------------
  Test_Result;

end Test_SI_Vectors;
