------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2004 Chris Holmes
-- Copyright (C) 2004, 2006, 2018, 2020, 2025 Christoph Grein
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

with Generic_SI.Generic_Vector_Space;

generic

  with package Real_Arrays is new Ada.Numerics.Generic_Real_Arrays  (Generic_SI.Real);
  with package Vectors     is new Generic_SI  .Generic_Vector_Space (Real_Arrays);
  use Real_Arrays, Vectors;

package Generic_SI.Generic_Quaternion_Space is

  --====================================================================
  -- Authors   Chris Holmes, Christoph Grein
  -- Version   6.0
  -- Date      16 April 2025
  --====================================================================
  -- Quaternions form a non-commutative field.
  --
  -- Quaternions are an extension of complex numbers. Instead of one
  -- imaginary part, they have three:
  --
  --    Q = s + v1 * i + v2 * j + v3 * k
  --
  -- with i * j = k = -j * i (valid for cyclic permutations of i, j,
  -- and k).
  -- They may be thought of being composed of a real scalar part (the
  -- real part), and an ordinary real three-dimensional vector (the
  -- imaginary part):
  --
  --    Q := (s, v)
  --
  -- Like for complex numbers, there is a complex conjugate
  --
  --    Q' := (s, -v)
  --
  -- Multiplication and inversion are defined as
  --
  --    Q1 * Q2 := (s1 * s2 - v1 . v2, s1 * v2 + s2 * v1 + v1 x v2)
  --     1 / Q  := Q' / (s**2 + v.v)
  --
  -- where "." stands for the dot product, "x" for the cross product.
  -- (For any v with only one component different from 0, this results
  -- in ordinary complex numbers.)
  -- Thus multiplication is associative, but not commutative.
  --
  -- The basic rules:
  --
  --    abs Q := Sqrt (Q * Q') = Sqrt (s**2 + v.v)
  --
  --    (Q1 * Q2)' = Q2' * Q1'  =>  abs (Q1 * Q2) = abs Q1 * abs Q2
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.H     1.0  18.03.2004 Baseline
  --  C.G.    2.0  26.03.2004 Reimplemented with vectors
  --  C.G.    2.1  21.04.2004 Mixed adding operations
  --  C.G.    2.2  23.04.2004 Made private to force consistent dimension
  --  C.G.    2.3  08.09.2004 Renamed Unit to Normalize to be consistent
  --                          with naming in Vector_Space
  --  C.G.    2.4  26.02.2006 pragma Pure
  --  C.G.    3.0  28.03.2006 use Ada.Numerics.Generic_Real_Arrays
  --  C.G.    4.0  04.08.2018 Unit strings
  --  C.G.    5.0  14.05.2020 Dimensions generic parameter
  --  C.G.    6.0  16.04.2025 Preconditions replace check in body
  --====================================================================

  type Quaternion is private;

  Null_Quaternion: constant Quaternion;
  One_Quaternion : constant Quaternion;
  I_Quaternion   : constant Quaternion;
  J_Quaternion   : constant Quaternion;
  K_Quaternion   : constant Quaternion;

  function Re (X: Quaternion) return Item;
  function Im (X: Quaternion) return Vector;

  function Compose (Re: Item; Im: Vector) return Quaternion with Pre => Same_Dimension (Re, Get (Im, 1)) or else raise Unit_Error;
  function Compose (Re: Item            ) return Quaternion;
  function Compose (          Im: Vector) return Quaternion;

  procedure Set_Re (X : in out Quaternion; Re: in Item  ) with Pre => Same_Dimension (Get (Im (X), 1), Re) or else raise Unit_Error;
  procedure Set_Im (X : in out Quaternion; Im: in Vector) with Pre => Same_Dimension (Re (X), Get (Im, 1)) or else raise Unit_Error;

  -- Quaternion operations

  function "abs"     (Right: Quaternion) return Item;        -- sqrt (s**2 + v**2)
  function Conjugate (Q    : Quaternion) return Quaternion;  -- (s, -v)
  function Inv       (Q    : Quaternion) return Quaternion;  -- 1 / Q
  function Normalize (Q    : Quaternion) return Quaternion;  -- abs = 1

  function "+" (Right: Quaternion) return Quaternion;
  function "-" (Right: Quaternion) return Quaternion;

  function "+" (Left, Right: Quaternion) return Quaternion;  -- with Pre implicit
  function "-" (Left, Right: Quaternion) return Quaternion;  -- as above
  function "*" (Left, Right: Quaternion) return Quaternion;
  function "/" (Left, Right: Quaternion) return Quaternion;

  function "+" (Left: Quaternion; Right: Item      ) return Quaternion;  -- with Pre
  function "+" (Left: Item      ; Right: Quaternion) return Quaternion;  --   implicit
  function "-" (Left: Quaternion; Right: Item      ) return Quaternion;  --     as
  function "-" (Left: Item      ; Right: Quaternion) return Quaternion;  --       above

  function "*" (Left: Quaternion; Right: Item      ) return Quaternion;
  function "*" (Left: Item      ; Right: Quaternion) return Quaternion;
  function "/" (Left: Quaternion; Right: Item      ) return Quaternion;
  function "/" (Left: Item      ; Right: Quaternion) return Quaternion;

private

  type Quaternion is record
    Re: Item;
    Im: Vector;
  end record;

  Null_Quaternion: constant Quaternion := (Zero, Null_Vector);
  One_Quaternion : constant Quaternion := ( One, Null_Vector);
  I_Quaternion   : constant Quaternion := (Zero, U1);
  J_Quaternion   : constant Quaternion := (Zero, U2);
  K_Quaternion   : constant Quaternion := (Zero, U3);

  pragma Inline("abs", "+", "-", "*", "/",
                Re, Im, Set_Re, Set_Im, Compose, Inv, Conjugate, Normalize);

end Generic_SI.Generic_Quaternion_Space;
