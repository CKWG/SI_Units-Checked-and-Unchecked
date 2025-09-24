------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2004, 2006, 2018, 2020, 2021, 2025
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
--   christ-Usch.grein@t-online.de
------------------------------------------------------------------------------

with Ada.Numerics.Generic_Real_Arrays;

generic

  with package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays (Generic_SI.Real);
  use Real_Arrays;

package Generic_SI.Generic_Vector_Space is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   8.0
  -- Date      16 April 2025
  --====================================================================
  -- This is not a linear algebraics package. All elements of a vector
  -- and a matrix must have the same dimension lest Unit_Error be
  -- raised.
  --
  -- Vectors can be defined in several ways, e.g.:
  --
  --   V: Vector := (1.0, 3.9, 0.0) * "m/s";
  --   V: Vector := (1.0 * U1 + 2.0 * U2) * "m/s";
  --   V: Vector := P1 * (1.0 * "m/s") + P2 * (2.0 * "m/s");
  --   V: Vector := 1.0 * "m/s" * P1 + 2.0 * "m/s" * P2;
  --
  -- Similarly for matrices.
  --
  -- (The inner and the cross products are not defined as operators
  -- since they are not associative with themselves or with one another
  -- and having both as operators would lead to ambiguities or errors.)
  --
  -- [Protovectors and -matrices are defined via the RM numerics annex
  -- Generic_Real_Arrays to ease using the additional functionality of
  -- that package.]
  --
  -- The operation to_Proto enables easy conversion from dimensioned
  -- to undimensioned vectors and matrices.
  --
  -- [Vectors and matrices are made private to prevent construction with
  -- differently dimensioned components.]
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  17.09.2002
  --  C.G.    1.1  08.11.2002 Added Det, Inv, Tran
  --  C.G.    1.2  17.11.2002 Use Real'Base
  --  C.G.    2.0  26.02.2004 Prototypes to ease construction
  --  C.G.    3.0  01.04.2004 Vector and Matrix are private; polar
  --  C.G.    4.0  16.02.2006 Polar now also private
  --  C.G.    4.1  26.02.2006 pragma Pure
  --  C.G.    4.2  27.03.2006 Added "/" (Left: Proto_x; Right: Item)
  --  C.G.    5.0  27.03.2006 use Ada.Numerics.Generic_Real_Arrays
  --  C.G.    6.0  03.08.2018 Unit strings
  --  C.G.    6.1  20.08.2018 Proto_Vector times unit strings
  --  C.G.    6.2  24.02.2020 Added a few vector and matrix subtypes
  --  C.G.    7.0  14.05.2020 Dimensions generic parameter
  --  C.G.    7.1  22.05.2020 Work-around for GNAT CE 2020 bug
  --  C.G.    7.2  28.05.2021 Work-around for [T521-020 public] removed
  --  C.G.    7.3  14.10.2021 Predicate_Failure for subtypes
  --  C.G.    8.0  18.04.2025 Preconditions replace check in body
 --====================================================================

  subtype Axis is Integer range 1 .. 3;

  -- Prototypes

  subtype Proto_Vector is Real_Vector (Axis);

  Null_Proto_Vector: constant                 Proto_Vector := (0.0, 0.0, 0.0);
  Unit_Proto_Vector: constant array (Axis) of Proto_Vector := (Unit_Vector (Index => 1, Order => Axis'Last),
                                                               Unit_Vector (Index => 2, Order => Axis'Last),
                                                               Unit_Vector (Index => 3, Order => Axis'Last));
  P1: Proto_Vector renames Unit_Proto_Vector (1);
  P2: Proto_Vector renames Unit_Proto_Vector (2);
  P3: Proto_Vector renames Unit_Proto_Vector (3);

  subtype Proto_Matrix is Real_Matrix (Axis, Axis);

  Null_Proto_Matrix: constant Proto_Matrix := ((0.0, 0.0, 0.0),
                                               (0.0, 0.0, 0.0),
                                               (0.0, 0.0, 0.0));
  Proto_Unity      : constant Proto_Matrix := Unit_Matrix (Order => Axis'Last);

  -- Vector

  type Vector  is private;
  type Vectors is array (Axis) of Vector;

  -- Check that X has the dimension given by Symbol (ignoring all prefixes)
  function has_Dimension (X: Vector; Symbol: String) return Boolean;

  -- Subtypes (add others here as needed)

  subtype Position              is Vector with Dynamic_Predicate => has_Dimension (Position             , "m"     ), Predicate_Failure => raise Unit_Error with "Position";
  subtype Velocity              is Vector with Dynamic_Predicate => has_Dimension (Velocity             , "m/s"   ), Predicate_Failure => raise Unit_Error with "Velocity";
  subtype Momentum_Vector       is Vector with Dynamic_Predicate => has_Dimension (Momentum_Vector      , "kg*m/s"), Predicate_Failure => raise Unit_Error with "Momentum_Vector";
  subtype Force_Vector          is Vector with Dynamic_Predicate => has_Dimension (Force_Vector         , "N"     ), Predicate_Failure => raise Unit_Error with "Force_Vector";
  subtype Electric_Field_Vector is Vector with Dynamic_Predicate => has_Dimension (Electric_Field_Vector, "V/m"   ), Predicate_Failure => raise Unit_Error with "Electric_Field_Vector";
  subtype Magnetic_Field_Vector is Vector with Dynamic_Predicate => has_Dimension (Magnetic_Field_Vector, "T"     ), Predicate_Failure => raise Unit_Error with "Magnetic_Field_Vector";

  Null_Vector: constant Vector;
  U1, U2, U3 : constant Vector;   -- the unit vectors
  Unit_Vector: constant Vectors;  --   along the axes

  function "*" (Left: Item        ; Right: Proto_Vector) return Vector;
  function "*" (Left: Proto_Vector; Right: Item        ) return Vector;
  function "/" (Left: Proto_Vector; Right: Item        ) return Vector;
  function "*" (Left: Proto_Vector; Right: String      ) return Vector;
  function "/" (Left: Proto_Vector; Right: String      ) return Vector;

  function Compose (V1, V2, V3: Item) return Vector with Pre => (Same_Dimension (V1, V2) and Same_Dimension (V2, V3)) or else raise Unit_Error;

  function Normalize (X: Proto_Vector) return Vector;  -- unit vector

  function  Get (V:        Vector; A:    Axis) return Item;
  procedure Set (V: in out Vector; A: in Axis; To: in Item) with Pre => Same_Dimension (Get (V, A), To) or else raise Unit_Error;

  type Polar is private;

  function Compose_Polar (R    : Item;   -- radius    non-negative
                          Phi  ,         -- longitude no constraint
                          Theta: Angle)  -- latitude  0 .. Pi
    return Polar;
  function Compose_Polar (R    : Item;   -- constraints as above
                          Phi  ,
                          Theta: Real)
    return Polar;

  function Radius    (P: Polar) return Item;   -- non-negative
  function Longitude (P: Polar) return Angle;  -- no constraint
  function Latitude  (P: Polar) return Angle;  -- 0 .. Pi

  procedure Set_Radius    (P: in out Polar; R    : in Item ) with Pre => Same_Dimension (Radius (P), R) or else raise Unit_Error;
  procedure Set_Longitude (P: in out Polar; Phi  : in Angle);  -- constraints as in
  procedure Set_Latitude  (P: in out Polar; Theta: in Angle);  --   Compose_Polar

  function To_Polar     (V: Vector) return Polar;  -- longitude in -Pi .. Pi
  function To_Cartesian (P: Polar ) return Vector;

  -- Matrix

  type Matrix is private;

  -- Check that X has the dimension given by Symbol (ignoring all prefixes)
  function has_Dimension (X: Matrix; Symbol: String) return Boolean;

  subtype Stress_Tensor is Matrix with Dynamic_Predicate => has_Dimension (Stress_Tensor, "Pa"), Predicate_Failure => raise Unit_Error with "Stress_Tensor";

  Null_Matrix: constant Matrix;
  Unity      : constant Matrix;

  -- Subtypes (add others here as needed)
  function "*" (Left: Item        ; Right: Proto_Matrix) return Matrix;
  function "*" (Left: Proto_Matrix; Right: Item        ) return Matrix;
  function "/" (Left: Proto_Matrix; Right: Item        ) return Matrix;
  function "*" (Left: Proto_Matrix; Right: String      ) return Matrix;
  function "/" (Left: Proto_Matrix; Right: String      ) return Matrix;

  function Compose (M11, M12, M13,
                    M21, M22, M23,
                    M31, M32, M33: Item) return Matrix with Pre => (Same_Dimension (M11, M12) and Same_Dimension (M12, M13) and
                                                                    Same_Dimension (M13, M21) and Same_Dimension (M21, M22) and Same_Dimension (M22, M23) and
                                                                    Same_Dimension (M23, M31) and Same_Dimension (M31, M32) and Same_Dimension (M32, M33)) or else raise Unit_Error;

  function  Get (M:        Matrix; A, B:    Axis) return Item;
  procedure Set (M: in out Matrix; A, B: in Axis; To: in Item) with Pre => not Same_Dimension (Get (M, A, B), To) or else raise Unit_Error;

  -- Vector algebra

  function "abs" (Right: Vector) return Item;

  function "+" (Right: Vector) return Vector;
  function "-" (Right: Vector) return Vector;

  function "+" (Left, Right: Vector) return Vector with Pre => Same_Dimension (Get (Left, 1), Get (Right, 1)) or else raise Unit_Error;
  function "-" (Left, Right: Vector) return Vector with Pre => Same_Dimension (Get (Left, 1), Get (Right, 1)) or else raise Unit_Error;

  function "*" (Left: Real'Base; Right: Vector   ) return Vector;
  function "*" (Left: Item     ; Right: Vector   ) return Vector;
  function "*" (Left: Vector   ; Right: Real'Base) return Vector;
  function "*" (Left: Vector   ; Right: Item     ) return Vector;
  function "/" (Left: Vector   ; Right: Real'Base) return Vector;
  function "/" (Left: Vector   ; Right: Item     ) return Vector;

  function Inner (Left, Right: Vector) return Item;    -- inner (dot) product
  function Cross (Left, Right: Vector) return Vector;  -- cross product

  -- Matrix algebra

  function "+" (Right: Matrix) return Matrix;
  function "-" (Right: Matrix) return Matrix;

  function "+" (Left, Right: Matrix) return Matrix with Pre => Same_Dimension (Get (Left, 1, 1), Get (Right, 1, 1)) or else raise Unit_Error;
  function "-" (Left, Right: Matrix) return Matrix with Pre => Same_Dimension (Get (Left, 1, 1), Get (Right, 1, 1)) or else raise Unit_Error;

  function "*" (Left: Real'Base; Right: Matrix   ) return Matrix;
  function "*" (Left: Item     ; Right: Matrix   ) return Matrix;
  function "*" (Left: Matrix   ; Right: Real'Base) return Matrix;
  function "*" (Left: Matrix   ; Right: Item     ) return Matrix;
  function "/" (Left: Matrix   ; Right: Real'Base) return Matrix;
  function "/" (Left: Matrix   ; Right: Item     ) return Matrix;

  function "*" (Left, Right: Matrix) return Matrix;

  function Det  (M: Matrix) return Item;    -- determinant
  function Inv  (M: Matrix) return Matrix;  -- inverse
  function Tran (M: Matrix) return Matrix;  -- transposed

  -- Mixed operations

  function "*" (Left: Matrix; Right: Vector) return Vector;  -- Sum (j) Lij * Rj
  function "*" (Left: Vector; Right: Matrix) return Vector;  -- Sum (i) Li  * Rij

  -- Conversion to prototypes (to_Proto (X) * Dim (X) = X)

  function to_Proto (V: Vector) return Proto_Vector;
  function to_Proto (M: Matrix) return Proto_Matrix;

  function Dim (V: Vector) return Item;  -- 1.0 with dimension
  function Dim (M: Matrix) return Item;  --     of argument

private

  pragma Inline ("+", "-", "*", "/", "abs",
                 Compose, Normalize, Get, Set,
                 Compose_Polar, Radius, Longitude, Latitude, Set_Radius, Set_Longitude, Set_Latitude,
                 To_Polar, To_Cartesian,
                 Inner, Cross,
                 Det, Inv, Tran,
                 to_Proto, Dim);

  type Vector is record
    Unit : Dimension;
    Value: Proto_Vector;
  end record;

  U1: constant Vector := (Dimensions.uno, P1);  -- don't know why uno is not directly visible
  U2: constant Vector := (Dimensions.uno, P2);
  U3: constant Vector := (Dimensions.uno, P3);
  Null_Vector: constant Vector  := (Dimensions.uno, Null_Proto_Vector);
  Unit_Vector: constant Vectors := (U1, U2, U3);

  type Polar is record
    Radius   : Item;  -- non-negative
    Longitude,        -- no constraint
    Latitude : Real;  -- 0 .. Pi
  end record;

  type Matrix is record
    Unit : Dimension;
    Value: Proto_Matrix;
  end record;

  Null_Matrix: constant Matrix := (Dimensions.uno, Null_Proto_Matrix);  -- don't know why uno is not directly visible
  Unity      : constant Matrix := (Dimensions.uno, Proto_Unity);

end Generic_SI.Generic_Vector_Space;
