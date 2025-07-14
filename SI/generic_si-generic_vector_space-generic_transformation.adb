------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2004, 2006, 2018, 2020, 2021 Christoph Grein
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
--   christ-usch.grein@t-online.de
------------------------------------------------------------------------------

package body Generic_SI.Generic_Vector_Space.Generic_Transformation is

  --====================================================================
  -- Authors   Christoph Grein
  -- Version   3.2
  -- Date      28 May 2021
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  23.04.2004
  --  C.G.    1.1  28.03.2006 Adapted to new Generic_Vector_Space
  --  C.G.    2.0  03.08.2018 Unit strings
  --  C.G.    3.0  14.05.2020 Dimensions generic parameter
  --  C.G.    3.1  22.05.2020 Work-around for GNAT CE 2020 bug
  --  C.G.    3.2  28.05.2021 Work-around for [T520-013 public] removed
  --====================================================================

  function Rotation (Axis: Vector; By: Angle) return Matrix is
    -- Use the direction cosines with respect to the three coordinate axes.
    Cos1      : constant Real := Axis.Value (1) / abs Axis.Value;
    Cos2      : constant Real := Axis.Value (2) / abs Axis.Value;
    Cos3      : constant Real := Axis.Value (3) / abs Axis.Value;
    CosA      : constant Real := Cos (By.Value);
    SinA      : constant Real := Sin (By.Value);
    CosA1     : constant Real := 1.0 - CosA;
    Cos1_CosA1: constant Real := Cos1 * CosA1;
    Cos2_CosA1: constant Real := Cos2 * CosA1;
    Cos3_CosA1: constant Real := Cos3 * CosA1;
  begin
    return (Unit  => uno,
            Value => ((        CosA + Cos1 * Cos1_CosA1, -Cos3 * SinA + Cos1 * Cos2_CosA1,  Cos2 * SinA + Cos1 * Cos3_CosA1),
                      ( Cos3 * SinA + Cos2 * Cos1_CosA1,         CosA + Cos2 * Cos2_CosA1, -Cos1 * SinA + Cos2 * Cos3_CosA1),
                      (-Cos2 * SinA + Cos3 * Cos1_CosA1,  Cos1 * SinA + Cos3 * Cos2_CosA1,         CosA + Cos3 * Cos3_CosA1)));
  end Rotation;

  function Rotation (Yaw, Pitch, Roll: Angle := 0.0 * "rad") return Matrix is
    --     ( CY , -SY , 0 )   (  CP , 0 , SP )   ( 1 , 0  ,  0  )
    -- M = ( SY ,  CY , 0 ) * (  0  , 1 , 0  ) * ( 0 , CR , -SR )
    --     ( 0  ,  0  , 1 )   ( -SP , 0 , CP )   ( 0 , SR ,  CR )
    CY: constant Real := Cos (Yaw  .Value);
    SY: constant Real := Sin (Yaw  .Value);
    CP: constant Real := Cos (Pitch.Value);
    SP: constant Real := Sin (Pitch.Value);
    CR: constant Real := Cos (Roll .Value);
    SR: constant Real := Sin (Roll .Value);
  begin
    return (Unit  => uno,
            Value => ((CY*CP, CY*SP*SR - SY*CR, CY*SP*CR + SY*SR),
                      (SY*CP, SY*SP*SR + CY*CR, SY*SP*CR - CY*SR),
                      (  -SP,    CP*SR        ,    CP*CR        )));
  end Rotation;

end Generic_SI.Generic_Vector_Space.Generic_Transformation;
