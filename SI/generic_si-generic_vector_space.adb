------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2004, 2006, 2018, 2020, 2025
--               Christoph Karl Walter Grein
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

package body Generic_SI.Generic_Vector_Space is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   8.0
  -- Date      15 April 2025
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  16.09.2002
  --  C.G.    1.1  08.11.2002 Added Det, Inv, Tran
  --  C.G.    1.2  17.11.2002 Use Real'Base
  --  C.G.    2.0  26.02.2004 Prototypes to ease construction; Adj
  --  C.G.    3.0  31.03.2004 Vector and Matrix are private
  --  C.G.    3.1  03.02.2006 Add range check for Polar.Longitude
  --  C.G.    4.0  16.02.2006 Polar now also private
  --  C.G.    4.1  27.03.2006 Added "/" (Left: Proto_x; Right: Item)
  --  C.G.    5.0  27.03.2006 use Ada.Numerics.Generic_Real_Arrays
  --  C.G.    6.0  03.08.2018 Unit strings
  --  C.G.    6.1  20.08.2018 Proto_Vector times unit strings
  --  C.G.    6.2  23.01.2020 Added a few vector and matrix subtypes
  --  C.G.    7.0  14.05.2020 Dimensions generic parameter
  --  C.G.    8.0  15.04.2025 Preconditions replace check in body
  --====================================================================

  function has_Dimension (X: Vector; Symbol: String) return Boolean is
  begin
    return has_Dimension (Get (X, 1), Symbol);
  end has_Dimension;

  function "*" (Left: Proto_Vector; Right: Item) return Vector is
  begin
    return (Right.Unit, Right.Value * Left);
  end "*";

  function "*" (Left: Item; Right: Proto_Vector) return Vector is
  begin
    return Right * Left;
  end "*";

  function "/" (Left: Proto_Vector; Right: Item) return Vector is
  begin
  --return (uno / Right.Unit, Left / Right.Value);
    return (Dimensions."/" (Dimensions.uno, Right.Unit), Left / Right.Value);  -- Why is this needed? [T520-013 public]
  end "/";

  function "*" (Left: Proto_Vector; Right: String) return Vector is
  begin
    return Left * (1.0*Right);
  end "*";

  function "/" (Left: Proto_Vector; Right: String) return Vector is
  begin
    return Left / (1.0*Right);
  end "/";

  function Compose (V1, V2, V3: Item) return Vector is
  begin
    return (V1.Unit, (V1.Value, V2.Value, V3.Value));
  end Compose;

  function Normalize (X: Proto_Vector) return Vector is
  begin
    return (X / abs X) * One;
  end Normalize;

  function Get (V: Vector; A: Axis) return Item is
  begin
    return (V.Unit, V.Value (A));
  end Get;

  procedure Set (V: in out Vector; A: in Axis; To: in Item) is
  begin
    V.Value (A) := To.Value;
  end Set;

  function Compose_Polar (R:Item; Phi, Theta: Angle) return Polar is
    -- No range check for longitude (Phi) necessary.
  begin
    return Compose_Polar (R, Value (Phi), Value (Theta));
  end Compose_Polar;

  function Compose_Polar (R    : Item;
                          Phi  ,
                          Theta: Real) return Polar is
  begin
    if Value (R) < 0.0 or Theta not in 0.0 .. Pi then
      raise Constraint_Error;
    end if;
    return (R, Phi, Theta);
  end Compose_Polar;

  function Radius (P: Polar) return Item is
  begin
    return P.Radius;
  end Radius;

  function Longitude (P: Polar) return Angle is
  begin
    return P.Longitude * One;
  end Longitude;

  function Latitude (P: Polar) return Angle is
  begin
    return P.Latitude * One;
  end Latitude;

  procedure Set_Radius (P: in out Polar; R: in Item) is
  begin
    if Value (R) < 0.0 then
      raise Constraint_Error;
    end if;
    P.Radius := R;
  end Set_Radius;

  procedure Set_Longitude (P: in out Polar; Phi: in Angle) is
  begin
    P.Longitude := Value (Phi);
  end Set_Longitude;

  procedure Set_Latitude (P: in out Polar; Theta: in Angle) is
  begin
    if Value (Theta) not in 0.0 .. Pi then
      raise Constraint_Error;
    end if;
    P.Latitude := Value (Theta);
  end Set_Latitude;

  function To_Polar (V: Vector) return Polar is
    Plane: constant Real := V.Value (1)**2 + V.Value (2)**2;
    Space: constant Real := Plane + V.Value (3)**2;
    R    : constant Item := (V.Unit, Sqrt (Space));
  begin
    if Space = 0.0 then
      return (Radius => R,
              others => 0.0);
    elsif Plane = 0.0 then
      return (Radius    => R,
              Longitude => 0.0,
              Latitude  => Arctan (Y => Sqrt (Plane), X => V.Value (3)));
    else
      return (Radius    => R,
              Longitude => Arctan (Y => V.Value (2) , X => V.Value (1)),
              Latitude  => Arctan (Y => Sqrt (Plane), X => V.Value (3)));
    end if;
  end To_Polar;

  function To_Cartesian (P: Polar) return Vector is
    V: constant Proto_Vector := (Sin (P.Latitude) * Cos (P.Longitude),
                                 Sin (P.Latitude) * Sin (P.Longitude),
                                 Cos (P.Latitude));
  begin
    return P.Radius * V;
  end To_Cartesian;

  function "abs" (Right: Vector) return Item is
  begin
    return (Right.Unit, abs Right.Value);
  end "abs";

  function "+" (Right: Vector) return Vector is
  begin
    return Right;
  end "+";

  function "-" (Right: Vector) return Vector is
  begin
    return (Right.Unit, -Right.Value);
  end "-";

  function "+" (Left, Right: Vector) return Vector is
  begin
    return (Left.Unit, Left.Value + Right.Value);
  end "+";

  function "-" (Left, Right: Vector) return Vector is
  begin
    return (Left.Unit, Left.Value - Right.Value);
  end "-";

  function "*" (Left: Real'Base; Right: Vector) return Vector is
  begin
    return (Right.Unit, Left * Right.Value);
  end "*";

  function "*" (Left: Item; Right: Vector) return Vector is
  begin
  --return (Left.Unit * Right.Unit, Left.Value * Right.Value);
    return (Dimensions."*" (Left.Unit, Right.Unit), Left.Value * Right.Value);  -- [T520-013 public]
  end "*";

  function "*" (Left: Vector; Right: Real'Base) return Vector is
  begin
    return Right * Left;
  end "*";

  function "*" (Left: Vector; Right: Item) return Vector is
  begin
    return Right * Left;
  end "*";

  function "/" (Left: Vector; Right: Real'Base) return Vector is
  begin
    return (Left.Unit, Left.Value / Right);
  end "/";

  function "/" (Left: Vector; Right: Item) return Vector is
  begin
  --return (Left.Unit / Right.Unit, Left.Value / Right.Value);
    return (Dimensions."/" (Left.Unit, Right.Unit), Left.Value / Right.Value);  -- [T520-013 public]
  end "/";

  function Inner (Left, Right: Vector) return Item is
  begin
  --return (Left.Unit * Right.Unit, Left.Value * Right.Value);
    return (Dimensions."*" (Left.Unit, Right.Unit), Left.Value * Right.Value);  -- [T520-013 public]
  end Inner;

  function Cross (Left, Right: Vector) return Vector is
  begin
    return (Unit  => --Left.Unit * Right.Unit,
                     Dimensions."*" (Left.Unit, Right.Unit),  -- [T520-013 public]
            Value => (Left.Value (2) * Right.Value (3) - Left.Value (3) * Right.Value (2),
                      Left.Value (3) * Right.Value (1) - Left.Value (1) * Right.Value (3),
                      Left.Value (1) * Right.Value (2) - Left.Value (2) * Right.Value (1)));
  end Cross;

  function has_Dimension (X: Matrix; Symbol: String) return Boolean is
  begin
    return has_Dimension (Get (X, 1, 1), Symbol);
  end has_Dimension;

  function "*" (Left: Proto_Matrix; Right: Item) return Matrix is
  begin
    return (Right.Unit, Left * Right.Value);
  end "*";

  function "*" (Left: Item; Right: Proto_Matrix) return Matrix is
  begin
    return Right * Left;
  end "*";

  function "/" (Left: Proto_Matrix; Right: Item) return Matrix is
  begin
  --return (uno / Right.Unit, Left / Right.Value);
    return (Dimensions."/" (Dimensions.uno, Right.Unit), Left / Right.Value);  -- [T520-013 public]
  end "/";

  function "*" (Left: Proto_Matrix; Right: String) return Matrix is
  begin
    return Left * (1.0*Right);
  end "*";

  function "/" (Left: Proto_Matrix; Right: String) return Matrix is
  begin
    return Left / (1.0*Right);
  end "/";

  function Compose (M11, M12, M13,
                    M21, M22, M23,
                    M31, M32, M33: Item) return Matrix is
  begin
    return (Unit  => M11.Unit,
            Value => ((M11.Value, M12.Value, M13.Value),
                      (M21.Value, M22.Value, M23.Value),
                      (M31.Value, M32.Value, M33.Value)));
  end Compose;

  function Get (M: Matrix; A, B: Axis) return Item is
  begin
    return (M.Unit, M.Value (A, B));
  end Get;

  procedure Set (M: in out Matrix; A, B: in Axis; To: in Item) is
  begin
    M.Value (A, B) := To.Value;
  end Set;

  function "+" (Right: Matrix) return Matrix is
  begin
    return Right;
  end "+";

  function "-" (Right: Matrix) return Matrix is
  begin
    return (Right.Unit, -Right.Value);
  end "-";

  function "+" (Left, Right: Matrix) return Matrix is
  begin
    return (Left.Unit, Left.Value + Right.Value);
  end "+";

  function "-" (Left, Right: Matrix) return Matrix is
  begin
    return (Left.Unit, Left.Value - Right.Value);
  end "-";

  function "*" (Left: Real'Base; Right: Matrix) return Matrix is
  begin
    return (Right.Unit, Left * Right.Value);
  end "*";

  function "*" (Left: Matrix; Right: Real'Base) return Matrix is
  begin
    return (Right * Left);
  end "*";

  function "/" (Left: Matrix; Right: Real'Base) return Matrix is
  begin
    return (Left.Unit, Left.Value / Right);
  end "/";

  function "*" (Left: Item; Right: Matrix) return Matrix is
  begin
  --return (Left.Unit * Right.Unit, Left.Value * Right.Value);
    return (Dimensions."*" (Left.Unit, Right.Unit), Left.Value * Right.Value);  -- [T520-013 public]
  end "*";

  function "*" (Left: Matrix; Right: Item) return Matrix is
  begin
    return Right * Left;
  end "*";

  function "/" (Left: Matrix; Right: Item) return Matrix is
  begin
  --return (Left.Unit / Right.Unit, Left.Value / Right.Value);
    return (Dimensions."/" (Left.Unit, Right.Unit), Left.Value / Right.Value);  -- [T520-013 public]
  end "/";

  function "*" (Left, Right: Matrix) return Matrix is
  begin
  --return (Left.Unit * Right.Unit, Left.Value * Right.Value);
    return (Dimensions."*" (Left.Unit, Right.Unit), Left.Value * Right.Value);  -- [T520-013 public]
  end "*";

  function Det (M: Matrix) return Item is
  begin
  --return (M.Unit**3, Determinant (M.Value));
    return (Dimensions."**" (M.Unit, 3), Determinant (M.Value));  -- [T520-013 public]
  end Det;

  function Inv (M: Matrix) return Matrix is
  begin
  --return (uno / M.Unit, Inverse (M.Value));
    return (Dimensions."/" (Dimensions.uno, M.Unit), Inverse (M.Value));  -- [T520-013 public]
  end Inv;

  function Tran (M: Matrix) return Matrix is
  begin
    return (M.Unit, Transpose (M.Value));
  end Tran;

  function "*" (Left: Matrix; Right: Vector) return Vector is
  begin
  --return (Left.Unit * Right.Unit, Left.Value * Right.Value);
    return (Dimensions."*" (Left.Unit, Right.Unit), Left.Value * Right.Value);  -- [T520-013 public]
  end "*";

  function "*" (Left: Vector; Right: Matrix) return Vector is
  begin
  --return (Left.Unit * Right.Unit, Left.Value * Right.Value);
    return (Dimensions."*" (Left.Unit, Right.Unit), Left.Value * Right.Value);  -- [T520-013 public]
  end "*";

  function to_Proto (V: Vector) return Proto_Vector is
  begin
    return V.Value;
  end to_Proto;

  function to_Proto (M: Matrix) return Proto_Matrix is
  begin
    return M.Value;
  end to_Proto;

  function Dim (V: Vector) return Item is
  begin
    return (V.Unit, 1.0);
  end Dim;

  function Dim (M: Matrix) return Item is
  begin
    return (M.Unit, 1.0);
  end Dim;

end Generic_SI.Generic_Vector_Space;
