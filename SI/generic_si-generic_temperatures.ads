------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2006, 2025 Christoph Karl Walter Grein
--
-- This program is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
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
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

generic
package Generic_SI.Generic_Temperatures is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.0
  -- Date      1 July 2025
  --====================================================================
  -- Kelvin and Celsius use the same unit interval.
  -- So do Rankine and Fahrenheit.
  -- Kelvin and Rankine are thermodynamic temperatures (they use the
  -- absolute zero as zero points).
  -- Celsius and Réaumur use the same fundamental distance between the
  -- ice point and the vapour point, but divide it into 100 resp. 80
  -- unit intervals.
  --
  -- Fahrenheit:  0°F = -17.78°C   32°F = 0°C     x°C = (9/5*x + 32)°F
  -- Rankine:     0°R = 0 K                       x K = (9/5)*x°R
  -- Celsius:     0°C = 273.15 K                  x K = (x - 273.15)°C
  -- Réaumur:     0°C = 0°R        80°R = 100°C   x°R = (5/4)*x°C
  --
  -- Note: Rankine and Réaumur use the same symbol °R in literature
  --       (formerly, Rankine was also denoted by °Rank resp. deg R).
  --       Since Réaumur has long been out of use, this does not lead
  --       to ambiguities.
  --
  -- Only Celsius and Fahrenheit are in use nowadays. Only Celsius is
  -- allowed in SI.
  --
  -- Discussion:
  -- Temperatures measured in non-thermodynamic units have a very
  -- limited range of applications. They basically serve only as a
  -- scale, but cannot be used in physical equations.
  -- If defined naively, nonsensical equations can be written.
  --
  -- Only the operations defined here seem to make sense to me:
  --
  -- It is nonsensical to add Celsius degrees, their difference in fact
  -- is a value in Kelvin; temperature differences can be added to
  -- Celsius.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  08.10.2002
  --  C.G.    1.1  26.02.2006 pragma Pure
  --  C.G.    2.0  01.07.2025 Resurrect unconstrained_checked_si-
  --                          generic_temperatures and rename; overhaul
  --====================================================================

  type Celsius is private;

  function "*" (Left: Real'Base; Right: String) return Celsius with Pre => Right = "°C" or else raise Unit_Error;

  function "+" (T: Celsius) return Celsius;
  function "-" (T: Celsius) return Celsius;

  function "+" (T: Celsius; Delta_T: Temperature) return Celsius;
  function "-" (Left, Right: Celsius) return Temperature;

  function "<"  (Left, Right: Celsius) return Boolean;
  function "<=" (Left, Right: Celsius) return Boolean;
  function ">=" (Left, Right: Celsius) return Boolean;
  function ">"  (Left, Right: Celsius) return Boolean;

  function to_Celsius (T: Temperature) return Celsius;
  function to_Kelvin  (T: Celsius    ) return Temperature;

  function Value (T: Celsius) return Real'Base;

private

  pragma Inline (to_Celsius, to_Kelvin,
                 Value,
                 "+", "-", "*",
                 "<", "<=", ">=", ">");

  type Celsius is record
    Value: Real'Base;
  end record;

  function "*" (Left: Real'Base; Right: String) return Celsius is (Value => Left);

  function "+" (T: Celsius) return Celsius is (T);
  function "-" (T: Celsius) return Celsius is (Value => -T.Value);

  function "+" (T: Celsius; Delta_T: Temperature) return Celsius     is (Value => T.Value + Value (Delta_T));
  function "-" (Left,       Right  : Celsius    ) return Temperature is ((Value (Left) - Value (Right)) * Kelvin);

  function "<"  (Left, Right: Celsius) return Boolean is (Left.Value <  Right.Value);
  function "<=" (Left, Right: Celsius) return Boolean is (Left.Value <= Right.Value);
  function ">=" (Left, Right: Celsius) return Boolean is (Left.Value >= Right.Value);
  function ">"  (Left, Right: Celsius) return Boolean is (Left.Value >  Right.Value);

  Zero_Celsius: constant := 273.15;

  function to_Celsius (T: Temperature) return Celsius     is (Value => T.Value - Zero_Celsius);
  function to_Kelvin  (T: Celsius    ) return Temperature is ((T.Value + Zero_Celsius) * Kelvin);

  function Value (T: Celsius) return Real'Base is (T.Value);

end Generic_SI.Generic_Temperatures;
