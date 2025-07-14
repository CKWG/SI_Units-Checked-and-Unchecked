------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2018, 2020 Christoph Karl Walter Grein
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

package body Generic_SI.Generic_Polynomials is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   5.0
  -- Date      14 May 2020
  --====================================================================
  --
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  07.12.1994
  --  C.G.    1.1  13.05.1997 Interface changed
  --  C.G.    2.0  31.07.1997 Interface rewrought
  --  C.G.    3.0  26.07.2002 Adapted to SI package
  --  C.G.    4.0  26.08.2018 Unit strings
  --  C.G.    5.0  14.05.2020 Dimensions generic parameter
  --====================================================================

  function Polynomial (Coeff: Coefficients; X: Item) return Item is
    -- Use Horner's scheme.
    F: Item := (Value => 0.0, Unit => Coeff (Coeff'Last).Unit / X.Unit);
  begin
    for I in reverse Coeff'Range loop
      F := F * X + Coeff (I);
    end loop;
    return F * X ** Coeff'First;
  end Polynomial;

  function Linear_Regression (S: Sample) return Straight_Line is
    Sx : Item := 0.0 * S (S'First).X;
    Sy : Item := 0.0 * S (S'First).Y;                  -- Get
    Sxy: Item := 0.0 * S (S'First).X * S (S'First).Y;  -- the
    Sx2: Item := 0.0 * S (S'First).X**2;               -- unit
  begin
    if S'Length <= 1 then
      raise Sample_Error;
    end if;
    for Si of S loop
      Sx  := Sx  + Si.X;
      Sy  := Sy  + Si.Y;
      Sxy := Sxy + Si.X * Si.Y;
      Sx2 := Sx2 + Si.X**2;
    end loop;
    declare
      N: constant Real := Real (S'Length);
      M: constant Item := (N * Sxy - Sx * Sy) / (N * Sx2 - Sx**2);
    begin
      return (0 => (Sy - M * Sx) / N,
              1 => M);
    end;
  end Linear_Regression;

  function Define (S: Sample) return Interpolation_Table is
    -- Check conditions on input tables.
    -- Make the internal table increasing and store direction in sign:
    --   Sign := +1 (increasing);
    --   Sign :=  0 (table error);
    --   Sign := -1 (decreasing).
    -- Liniear interpolation will be called with signed argument "Sign * X".
    Table: Interpolation_Table (S'Length);
  begin
    if S'Length <= 1 then
      raise Sample_Error;
    end if;
    -- Check monotony
    if S (S'First).X < S (S'First + 1).X then
      Table.Sign := +1.0;  -- increasing
    else
      Table.Sign := -1.0;  -- decreasing
    end if;
    Table.S (1) := (X => Table.Sign * S (S'First).X,
                    Y =>              S (S'First).Y);
    for I in 2 .. Table.Length loop
      Table.S (I) := (X => Table.Sign * S (S'First + I - 1).X,
                      Y =>              S (S'First + I - 1).Y);
      if Table.S (I - 1).X >= Table.S (I).X then
        raise Sample_Error;
      end if;
    end loop;
    return Table;
  end Define;

  function Linear_Interpolation
               (Table: Interpolation_Table; X: Item) return Item is
    -- Check whether table is defined.
    -- Use a binary search to find X's position in the table.
    Signed_X: constant Item := Table.Sign * X;
    Lower, Upper: Positive;
    function Binary_Search return Positive is
      L: Positive := 1;
      U: Positive := Table.Length;
      M: Positive;
    begin
      while U - L > 1 loop
        M := Integer (L + U) / 2;
        if Signed_X <= Table.S (M).X then
          U := M;
        else
          L := M;
        end if;
      end loop;
      return L;
    end Binary_Search;
  begin
    if Table.Sign = 0.0 then
      raise Sample_Error;
    end if;
    if Signed_X < Table.S (1).X then
      Lower := 1;
    elsif Signed_X > Table.S (Table.Length).X then
      Lower := Table.Length - 1;
    else
      Lower := Binary_Search;
    end if;
    Upper := Lower + 1;
    return  Table.S (Lower).Y +
           (Table.S (Upper).Y - Table.S (Lower).Y) * ((         Signed_X - Table.S (Lower).X) /
                                                      (Table.S (Upper).X - Table.S (Lower).X));
  end Linear_Interpolation;

end Generic_SI.Generic_Polynomials;
