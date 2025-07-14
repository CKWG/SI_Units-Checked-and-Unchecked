------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2004, 2006, 2018 Christoph Grein
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
-- Software's homepage and email address:
--   http://www.christ-usch-grein.homepage.t-online.de/
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

with Ada.Text_IO;
use  Ada.Text_IO;
with Ada.Strings.Fixed;

with SI.IO,
     SI.VS.Trans,
     SI.QS.Trans;
use  SI.IO, SI,
     SI.VS, SI.VS.Trans,
     SI.QS, SI.QS.Trans;

procedure Decompose_Rotation is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      4 August 2018
  --====================================================================
  -- Decompose a rotation around a given axis into separate rotations
  -- around the coordinate axes, once with matrices, once with
  -- quaternions.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  20.07.2004
  --  C.G.    1.1  28.03.2006 Adapted to new Generic_Vector_Space
  --  C.G.    2.0  04.08.2018 Unit strings
  --====================================================================

  Degree: constant Angle := Pi/180.0 * "rad";
  Phi   : constant Angle := 10.0 * Degree;

  Axis: constant Vector := (4.0, 3.0, 12.0) * One;

  RotY: constant Matrix := Rotation (Yaw   => -Arctan (Y => Get (Axis, 2), X => Get (Axis, 1)));
  RotP: constant Matrix := Rotation (Pitch => -Arccos (Get (Axis, 3) / abs Axis));
  RotA: constant Matrix := Rotation (Yaw   => Phi);

  QuY: constant Quaternion := Rotation (Yaw   => -Arctan (Y => Get (Axis, 2), X => Get (Axis, 1)));
  QuP: constant Quaternion := Rotation (Pitch => -Arccos (Get (Axis, 3) / abs Axis));
  QuA: constant Quaternion := Rotation (Yaw   => Phi);

  Original: constant Vector := U1;

  procedure Print (V: in Vector) is
    Col: constant Ada.Text_IO.Count := Ada.Text_IO.Col;
    use Ada.Strings, Ada.Strings.Fixed;
  begin
    for I in VS.Axis loop
      Set_Col (Col);
      Put ("(");  Put (Trim (VS.Axis'Image (I), Left));  Put (")");  Put (Get (V, I));
      New_Line;
    end loop;
  end Print;

begin

  Put_Line ("Example Rotation Decomposition");
  New_Line;

  Put_Line ("Original vector");
  Print    (Original);
  New_Line;

  -- Matrices -------------------------------------------------------------

  Put_Line ("Successive Matrix Rotations");
  New_Line;

  Put_Line ("Matrix -Yaw (rotate Axis around z axis => Axis' above x axis");
  Print (RotY * Original);
  New_Line;

  Put_Line ("Matrix -Pitch (rotate Axis' arond y axis => Axis"" on z axis");
  Print (RotP * RotY * Original);  -- Now the rotational axis is along z
  New_Line;

  Put_Line ("Matrix Phi - rotate arond Axis"" (z axis) by Phi");
  Print (RotA * RotP * RotY * Original);
  New_Line;

  Put_Line ("Matrix Pitch - revert second");
  Print (Tran (RotP) * RotA * RotP * RotY * Original);
  New_Line;

  Put_Line ("Matrix Yaw - revert first => Rotation around Axis by Phi complete");
  Print (Tran (RotY) * Tran (RotP) * RotA * RotP * RotY * Original);
  New_Line;

  -- This is the same as:

  Put_Line ("Matrix Rotated directly around Axis");
  Print    (Rotation (Axis, Phi) * Original);
  New_Line;

  -- Quaternions ----------------------------------------------------------

  Put_Line ("Quaternion -Yaw");
  Print (Im (QuY * Compose (Original) * Conjugate (QuY)));
  New_Line;

  Put_Line ("Quaternion -Pitch");
  Print (Im (QuP *QuY * Compose (Original) *
             Conjugate (QuY) * Conjugate (QuP)));
  New_Line;

  Put_Line ("Quaternion Phi");
  Print (Im (QuA * QuP * QuY * Compose (Original) *
             Conjugate (QuY) * Conjugate (QuP) * Conjugate (QuA)));
  New_Line;

  Put_Line ("Quaternion Pitch");
  Print (Im (Conjugate (QuP) * QuA * QuP * QuY * Compose (Original) *
             Conjugate (QuY) * Conjugate (QuP) * Conjugate (QuA) * QuP));
  New_Line;

  Put_Line ("Quaternion Yaw");
  Print (Im (Conjugate (QuY) * Conjugate (QuP) * QuA * QuP * QuY * Compose (Original) *
             Conjugate (QuY) * Conjugate (QuP) * Conjugate (QuA) * QuP * QuY));
  New_Line;

  -- This is the same as:

  Put_Line ("Quaternion Rotated");
  Print    (Im (Rotation (Axis, Phi) * Compose (Original) * Conjugate (Rotation (Axis, Phi))));
  New_Line;

  Put_Line ("Example completed!");

end Decompose_Rotation;
