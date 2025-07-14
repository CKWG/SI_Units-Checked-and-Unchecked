------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2004 Chris Holmes
-- Copyright (C) 2004, 2018, 2020 Christoph Grein
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
-- Source:
-- https://www.adaic.org/ada-resources/tools-libraries/
--   (see Christoph Grein's Essentials)
-- http://archive.adaic.com/tools/CKWG/Dimension/Dimension.html
--
-- Author's email address:
--   christ-Usch.grein@t-online.de
------------------------------------------------------------------------------

package body Generic_SI.Generic_Quaternion_Space.Generic_Transformation is

  --====================================================================
  -- Authors   Chris Holmes, Christoph Grein
  -- Version   3.0
  -- Date      15 May 2020
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  CKH     0.0  18.03.2004 Prototype for rotation around the three
  --                          coordinate axes
  --  C.G.    1.0  10.05.2004 Function wrapped in child package
  --  C.G.    1.1  06.09.2004 Rotation around an axis added
  --  C.G.    2.0  04.08.2018 Unit strings
  --  C.G.    3.0  15.05.2020 Dimensions generic parameter
  --====================================================================

  function Rotation (Axis: Vector; By: Angle) return Quaternion is
    A: constant Angle := By/2.0;
  begin
    return (Re => Cos (A), Im => Sin (A) * Axis / abs Axis);
  end Rotation;

  function Rotation (Yaw, Pitch, Roll: Angle := 0.0 * "rad") return Quaternion is
    -- Q = (C3, (0, 0, S3)) * (C2, (0, S2, 0)) * (C1, (S1, 0, 0))
    R : constant Angle := Roll /2.0;
    P : constant Angle := Pitch/2.0;
    Y : constant Angle := Yaw  /2.0;
    C1: constant Item := Cos (R);
    S1: constant Item := Sin (R);
    C2: constant Item := Cos (P);
    S2: constant Item := Sin (P);
    C3: constant Item := Cos (Y);
    S3: constant Item := Sin (Y);
  begin
    return (Re =>                C1*C2*C3 + S1*S2*S3,
            Im => Compose (V1 => S1*C2*C3 - C1*S2*S3,
                           V2 => C1*S2*C3 + S1*C2*S3,
                           V3 => C1*C2*S3 - S1*S2*C3));
  end Rotation;

end Generic_SI.Generic_Quaternion_Space.Generic_Transformation;
