------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2004, 2006, 2018, 2020 Christoph Grein
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

generic
package Generic_SI.Generic_Quaternion_Space.Generic_Transformation is

  --====================================================================
  -- Authors   Christoph Grein
  -- Version   3.0
  -- Date      15 May 2020
  --====================================================================
  -- A rotation of a vector V via a quaternion Q is achieved by trans-
  -- forming V with the following equation:
  --
  --   VQ' := Q * VQ / Q
  --
  -- or for a normalised quaternion (1/Q = Conjugate (Q)):
  --
  --   VQ' := Q * VQ * Conjugate (Q)
  --
  -- where Re (VQ) = 0, Im (VQ) = V, and the rotated vector V' is the
  -- imaginary part of VQ':
  --
  --   V' = Im (VQ')
  --
  -- Note that the vector is rotated, not the coordinate system.
  --
  -- The functions provided deliver normalised quaternions.
  --
  -- 1. Rotation (right-handed) around an axis given by a vector.
  --
  --    function Rotation (Axis: Vector; By: Angle) return Quaternion;
  --
  -- 2. Successive rotation (right-handed) around the three coordinate
  --    axes.
  --
  --    function Rotation (Yaw, Pitch, Roll: Angle := 0.0 * "rad")
  --      return Quaternion;
  --
  --    Roll : around x-axis (first rotation)
  --    Pitch: around y-axis (second rotation)
  --    Yaw  : around z-axis (third rotation)
  --
  --    This transformation Q fulfills the equation
  --
  --    Q (Yaw, Pitch, Roll) = Q (Yaw) * Q (Pitch) * Q (Roll)
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  10.05.2004
  --  C.G.    1.1  20.07.2004 Rotation around an axis added
  --  C.G.    1.2  26.02.2006 pragma Pure
  --  C.G.    2.0  04.08.2018 Unit strings
  --  C.G.    3.0  15.05.2020 Dimensions generic parameter
  --====================================================================

  use Vectors;

  function Rotation (Axis: Vector; By: Angle) return Quaternion;

  function Rotation (Yaw, Pitch, Roll: Angle := 0.0 * "rad") return Quaternion;

end Generic_SI.Generic_Quaternion_Space.Generic_Transformation;
