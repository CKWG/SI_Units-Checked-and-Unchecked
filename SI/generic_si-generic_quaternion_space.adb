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
-- Author's email address:
--   christ-Usch.grein@t-online.de
------------------------------------------------------------------------------

package body Generic_SI.Generic_Quaternion_Space is

  --====================================================================
  -- Authors   Chris Holmes, Christoph Grein
  -- Version   5.0
  -- Date      15 April 2025
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.H     1.0  18.03.2004 Baseline.
  --  C.G.    2.0  30.03.2004 Reimplemented with vectors
  --  C.G.    2.1  21.04.2004 Mixed adding operations
  --  C.G.    2.2  23.04.2004 Made private to force consistent dimension
  --  C.G.    2.3  08.09.2004 Renamed Unit to Normalize to be consistent
  --                          with naming in Vector_Space
  --  C.G.    3.0  04.08.2018 Unit strings
  --  C.G.    4.0  14.05.2020 Dimensions generic parameter
  --  C.G.    5.0  15.04.2025 Preconditions replace check in body
  --====================================================================

  function Re (X: Quaternion) return Item is
  begin
    return X.Re;
  end Re;

  function Im (X: Quaternion) return Vector is
  begin
    return X.Im;
  end Im;

  function Compose (Re: Item; Im: Vector) return Quaternion is
  begin
    return (Re, Im);
  end Compose;

  function Compose (Re: Item) return Quaternion is
  begin
    return (Re, Null_Vector * (Re.Unit, 0.0));
  end Compose;

  function Compose (Im: Vector) return Quaternion is
  begin
    return ((Get (Im, 1).Unit, 0.0), Im);
  end Compose;

  procedure Set_Re (X : in out Quaternion; Re: in Item) is
  begin
    X.Re := Re;
  end Set_Re;

  procedure Set_Im (X : in out Quaternion; Im: in Vector) is
  begin
    X.Im := Im;
  end Set_Im;

  function "abs" (Right: Quaternion) return Item is
  begin
    return Sqrt (Right.Re**2 + Inner (Right.Im, Right.Im));
  end "abs";

  function Conjugate (Q: Quaternion) return Quaternion is
  begin
    return (Q.Re, -Q.Im);
  end Conjugate;

  function Inv (Q: Quaternion) return Quaternion is
  begin
    return Conjugate (Q) / (Q.Re**2 + Inner (Q.Im, Q.Im));
  end Inv;

  function Normalize (Q: Quaternion) return Quaternion is
  begin
    return Q / abs Q;
  end Normalize;

  function "+" (Right: Quaternion) return Quaternion is
  begin
    return Right;
  end "+";

  function "-" (Right: Quaternion) return Quaternion is
  begin
    return (-Right.Re, -Right.Im);
  end "-";

  function "+" (Left, Right: Quaternion) return Quaternion is
  begin
    return (Left.Re + Right.Re, Left.Im + Right.Im);
  end "+";

  function "-" (Left, Right: Quaternion) return Quaternion is
  begin
    return (Left.Re - Right.Re, Left.Im - Right.Im);
  end "-";

  function "*" (Left, Right: Quaternion) return Quaternion is
  begin
    return (Left.Re * Right.Re - Inner (Left.Im, Right.Im),
            Left.Re * Right.Im + Left.Im * Right.Re + Cross (Left.Im, Right.Im));
  end "*";

  function "/" (Left, Right: Quaternion) return Quaternion is
  begin
    return Left * Inv (Right);
  end "/";

  -- See RM G1.1(56,57)

  function "+" (Left: Quaternion; Right: Item) return Quaternion is
  begin
    return (Left.Re + Right, Left.Im);
  end "+";

  function "+" (Left: Item; Right: Quaternion) return Quaternion is
  begin
    return (Left + Right.Re, Right.Im);
  end "+";

  function "-" (Left: Quaternion; Right: Item) return Quaternion is
  begin
    return (Left.Re - Right, Left.Im);
  end "-";

  function "-" (Left: Item; Right: Quaternion) return Quaternion is
  begin
    return (Left - Right.Re, -Right.Im);
  end "-";

  function "*" (Left: Quaternion; Right: Item) return Quaternion is
  begin
    return (Left.Re * Right, Left.Im * Right);
  end "*";

  function "*" (Left: Item; Right: Quaternion) return Quaternion is
  begin
    return Right * Left;
  end "*";

  function "/" (Left: Quaternion; Right: Item) return Quaternion is
  begin
    return (Left.Re / Right, Left.Im / Right);
  end "/";

  function "/" (Left: Item; Right: Quaternion) return Quaternion is
  begin
    return Left * Inv (Right);
  end "/";

end Generic_SI.Generic_Quaternion_Space;
