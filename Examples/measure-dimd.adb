------------------------------------------------------------------------------
-- Checked and Generic Computation with SI Units
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

with SI.Nat;
with SI.VS.Trans,
     SI.QS.Trans;

separate (Measure)
package body Dimd is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.0
  -- Date      15 June 2025
  --====================================================================
  -- The dimensioned version.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  15.06.2025 Made separate
  --====================================================================

  use SI.Nat;
  use SI.VS, SI.VS.Trans,
      SI.QS, SI.QS.Trans;

  function Schottky_Langmuir (Volt: Voltage; Dist: Length) return Current_Density is
  begin
    return (4.0/9.0) * Eps_0 * Sqrt (2.0 * Elementary_Charge / Electron_Mass) *
           Volt**(3/2) / Dist**2;
  end Schottky_Langmuir;

  SchL: constant Item := (4.0/9.0) * Eps_0 * Sqrt (2.0 * Elementary_Charge / Electron_Mass);

  function Schottky_Langmuir_Opt (Volt: Voltage; Dist: Length) return Current_Density is
  begin
    return SchL * Volt**(3/2) / Dist**2;
  end Schottky_Langmuir_Opt;

  Degree  : constant Angle      := Pi/180.0 * "rad";
  Roll    : constant Angle      := 10.0 * Degree;
  Pitch   : constant Angle      := 20.0 * Degree;
  Yaw     : constant Angle      := 30.0 * Degree;
  RotatorM: constant Matrix     := Rotation (Yaw, Pitch, Roll);
  RotatorQ: constant Quaternion := Rotation (Yaw, Pitch, Roll);
  InvRotQ : constant Quaternion := Inv (RotatorQ);
  Original: constant Vector     := (400.0, 300.0, 200.0) * One;

  procedure Matrix_Rotate is
    Rotated: Vector;
  begin
    Rotated := RotatorM * Original;
  end Matrix_Rotate;

  procedure Quaternion_Rotate is
    Rotated: Quaternion;
  begin
    Rotated := RotatorQ * Compose (Original) * InvRotQ;
  end Quaternion_Rotate;

end Dimd;
