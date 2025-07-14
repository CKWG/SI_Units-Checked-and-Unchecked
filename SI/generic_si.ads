------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2003, 2005, 2006, 2011, 2018, 2020, 2021, 2025
-- Christoph Karl Walter Grein
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

with Ada.Numerics;
private with Ada.Numerics.Generic_Elementary_Functions;

with Rational_Arithmetics;
use  Rational_Arithmetics;

with Dimension_Signature;

generic

  type Real is digits <>;

  with package Dimensions is new Dimension_Signature (<>);
  use Dimensions;

package Generic_SI is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   6.0
  -- Date      18 April 2025
  --====================================================================
  -- Magnetic_Flux_Density is the classical name. Nowadays it's often
  -- just called Magnetic_Field.
  --
  -- Dimensioned units are written as a real number multiplied or
  -- divided by a unit string. All unit symbols may be used together
  -- with prefixes and exponents as defined by SI (case sensitive).
  -- Exponents may be signed whole or rational numbers.
  -- For the exact syntax of unit strings see package Generic_Text_IO.
  --
  -- Examples:
  --   1.0*""                        one dimensionless (unequal to 1.0)
  --   1.0*"mm"                      one millimeter
  --  10.0/"ms"                      ten per millisecond
  --   5.0*"MS"                      five megasiemens
  --   1.0*"km**2"                   one square kilometer
  --   1.0*"cm**(3/2)*g(1/2)*s(-1)"  esu (Gaussian electrostatic unit)
  --   6.674_2E-11*"m**3/(kg*s**2)"  gravitational constant
  --
  -- Not allowed:
  --   1.0*"(km/s)**2"               parentheses in units only after /
  --   6.674_2E-11*"m**3/kg/s**2"    only one / in units
  --
  -- Note:
  -- For Text_IO, units to use in output may be given with leading
  -- operator (see child package Generic_Text_IO):
  --   Put (1000.0*"m/s", Dim => "*km/s");
  --   Put (   1.0*"Bq" , Dim => "/s"   );
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  10.07.2002
  --  C.G.    1.1  15.11.2002 Use Real'Base
  --  C.G.    1.2  21.02.2003 Add Dimension_as
  --  C.G.    1.3  28.11.2005 Binary prefixes removed
  --  C.G.    1.4  13.01.2006 Use None in Zero and One
  --  C.G.    1.5  26.02.2006 pragma Pure
  --  C.G.    2.0  20.03.2006 private with Generic_Elementary_Functions
  --  C.G.    3.0  13.12.2011 New subpackage for_Test_only
  --  C.G.    4.0  28.07.2018 Base units and prefixes removed from
  --                          public part, replaced by unit strings
  --  C.G.    4.1  10.08.2018 Bug fix in inverse trigonometric functions
  --  C.G.    4.2  27.08.2018 Bug fix in comment
  --  C.G.    4.3  31.08.2018 Dimensionless ""
  --  C.G.    4.4  28.09.2018 Value returns Real'Base
  --  C.G.    4.5  15.10.2018 kat replaces mol/s
  --  C.G.    4.6  23.02.2020 Electric_Field added; Magnetic_Field as a
  --                          renaming of Magnetic_Flux_Density
  --  C.G.    5.0  13.05.2020 Dimension now generic parameter; renamed
  --                          Generic_SI from Checked_SI; no change in
  --                          visible part
  --  C.G.    5.1  22.05.2020 Work-around for GNAT CE 2020 bug
  --  C.G.    5.2  28.05.2021 Work-around for [T521-020 public] removed
  --  C.G.    5.3  14.10.2021 Predicate_Failure for subtypes
  --  C.G.    6.0  18.04.2025 Preconditions replace check in body
  --====================================================================

  type Item is private;

  Zero: constant Item;  -- 0.0*""
  One : constant Item;  -- 1.0*""

  -- Check that the two items have the same dimension
  function Same_Dimension (X, Y: Item) return Boolean;
  -- Check that X has the dimension given by Symbol (ignoring all prefixes)
  function has_Dimension (X: Item; Symbol: String) return Boolean;

  function Value (X: Item) return Real'Base;

  Unit_Error: exception;

  -- Subtypes (add here, if needed, other units like angular momentum)

  subtype Dimensionless           is Item with Dynamic_Predicate => has_Dimension (Dimensionless          , ""        ), Predicate_Failure => raise Unit_Error with "Dimensionless";
  subtype Angle                   is Item with Dynamic_Predicate => has_Dimension (Angle                  , "rad"     ), Predicate_Failure => raise Unit_Error with "Angle";
  subtype Solid_Angle             is Item with Dynamic_Predicate => has_Dimension (Solid_Angle            , "sr"      ), Predicate_Failure => raise Unit_Error with "Solid_Angle";

  subtype Length                  is Item with Dynamic_Predicate => has_Dimension (Length                 , "m"       ), Predicate_Failure => raise Unit_Error with "Length";
  subtype Area                    is Item with Dynamic_Predicate => has_Dimension (Area                   , "m**2"    ), Predicate_Failure => raise Unit_Error with "Area";
  subtype Volume                  is Item with Dynamic_Predicate => has_Dimension (Volume                 , "m**3"    ), Predicate_Failure => raise Unit_Error with "Volume";

  subtype Time                    is Item with Dynamic_Predicate => has_Dimension (Time                   , "s"       ), Predicate_Failure => raise Unit_Error with "Time";
  subtype Frequency               is Item with Dynamic_Predicate => has_Dimension (Frequency              , "Hz"      ), Predicate_Failure => raise Unit_Error with "Frequency";
  subtype Angular_Frequency       is Item with Dynamic_Predicate => has_Dimension (Angular_Frequency      , "rad/s"   ), Predicate_Failure => raise Unit_Error with "Angular_Frequency";
  subtype Speed                   is Item with Dynamic_Predicate => has_Dimension (Speed                  , "m/s"     ), Predicate_Failure => raise Unit_Error with "Speed";
  subtype Acceleration            is Item with Dynamic_Predicate => has_Dimension (Acceleration           , "m/s**2"  ), Predicate_Failure => raise Unit_Error with "Acceleration";

  subtype Mass                    is Item with Dynamic_Predicate => has_Dimension (Mass                   , "kg"      ), Predicate_Failure => raise Unit_Error with "Mass";
  subtype Mass_Density            is Item with Dynamic_Predicate => has_Dimension (Mass_Density           , "kg/m**3" ), Predicate_Failure => raise Unit_Error with "Mass_Density";
  subtype Momentum                is Item with Dynamic_Predicate => has_Dimension (Momentum               , "kg*m/s"  ), Predicate_Failure => raise Unit_Error with "Momentum";

  subtype Force                   is Item with Dynamic_Predicate => has_Dimension (Force                  , "N"       ), Predicate_Failure => raise Unit_Error with "Force";
  subtype Torque                  is Item with Dynamic_Predicate => has_Dimension (Torque                 , "N*m"     ), Predicate_Failure => raise Unit_Error with "Torque";
  subtype Pressure                is Item with Dynamic_Predicate => has_Dimension (Pressure               , "Pa"      ), Predicate_Failure => raise Unit_Error with "Pressure";
  subtype Energy                  is Item with Dynamic_Predicate => has_Dimension (Energy                 , "J"       ), Predicate_Failure => raise Unit_Error with "Energy";
  subtype Power                   is Item with Dynamic_Predicate => has_Dimension (Power                  , "W"       ), Predicate_Failure => raise Unit_Error with "Power";

  subtype Current                 is Item with Dynamic_Predicate => has_Dimension (Current                , "A"       ), Predicate_Failure => raise Unit_Error with "Current";
  subtype Current_Density         is Item with Dynamic_Predicate => has_Dimension (Current_Density        , "A/m**2"  ), Predicate_Failure => raise Unit_Error with "Current_Density";
  subtype Charge                  is Item with Dynamic_Predicate => has_Dimension (Charge                 , "C"       ), Predicate_Failure => raise Unit_Error with "Charge";
  subtype Voltage                 is Item with Dynamic_Predicate => has_Dimension (Voltage                , "V"       ), Predicate_Failure => raise Unit_Error with "Voltage";
  subtype Capacitance             is Item with Dynamic_Predicate => has_Dimension (Capacitance            , "F"       ), Predicate_Failure => raise Unit_Error with "Capacitance";
  subtype Resistance              is Item with Dynamic_Predicate => has_Dimension (Resistance             , "Ohm"     ), Predicate_Failure => raise Unit_Error with "Resistance";
  subtype Conductance             is Item with Dynamic_Predicate => has_Dimension (Conductance            , "S"       ), Predicate_Failure => raise Unit_Error with "Conductance";

  subtype Electric_Field          is Item with Dynamic_Predicate => has_Dimension (Electric_Field         , "V/m"     ), Predicate_Failure => raise Unit_Error with "Electric_Field";  -- E field

  subtype Inductance              is Item with Dynamic_Predicate => has_Dimension (Inductance             , "H"       ), Predicate_Failure => raise Unit_Error with "Inductance";
  subtype Magnetic_Field_Strength is Item with Dynamic_Predicate => has_Dimension (Magnetic_Field_Strength, "A/m"     ), Predicate_Failure => raise Unit_Error with "Magnetic_Field_Strength";  -- H field
  subtype Magnetic_Flux           is Item with Dynamic_Predicate => has_Dimension (Magnetic_Flux          , "Wb"      ), Predicate_Failure => raise Unit_Error with "Magnetic_Flux";
  subtype Magnetic_Flux_Density   is Item with Dynamic_Predicate => has_Dimension (Magnetic_Flux_Density  , "T"       ), Predicate_Failure => raise Unit_Error with "Magnetic_Flux_Density";  -- B field
  subtype Magnetic_Field          is      Magnetic_Flux_Density;  --

  subtype Temperature             is Item with Dynamic_Predicate => has_Dimension (Temperature            , "K"       ), Predicate_Failure => raise Unit_Error with "Temperature";

  subtype Luminous_Intensity      is Item with Dynamic_Predicate => has_Dimension (Luminous_Intensity     , "cd"      ), Predicate_Failure => raise Unit_Error with "Luminous_Intensity";
  subtype Luminance               is Item with Dynamic_Predicate => has_Dimension (Luminance              , "cd/m**2" ), Predicate_Failure => raise Unit_Error with "Luminance";
  subtype Luminous_Flux           is Item with Dynamic_Predicate => has_Dimension (Luminous_Flux          , "lm"      ), Predicate_Failure => raise Unit_Error with "Luminous_Flux";
  subtype Illuminance             is Item with Dynamic_Predicate => has_Dimension (Illuminance            , "lx"      ), Predicate_Failure => raise Unit_Error with "Illuminance";

  subtype Amount_of_Substance     is Item with Dynamic_Predicate => has_Dimension (Amount_of_Substance    , "mol"     ), Predicate_Failure => raise Unit_Error with "Amount_of_Substance";
  subtype Concentration           is Item with Dynamic_Predicate => has_Dimension (Concentration          , "mol/m**3"), Predicate_Failure => raise Unit_Error with "Concentration";
  subtype Catalytic_Activity      is Item with Dynamic_Predicate => has_Dimension (Catalytic_Activity     , "kat"     ), Predicate_Failure => raise Unit_Error with "Catalytic_Activity";

  subtype Radio_Activity          is Item with Dynamic_Predicate => has_Dimension (Radio_Activity         , "Bq"      ), Predicate_Failure => raise Unit_Error with "Radio_Activity";
  subtype Absorbed_Dose           is Item with Dynamic_Predicate => has_Dimension (Absorbed_Dose          , "Gy"      ), Predicate_Failure => raise Unit_Error with "Absorbed_Dose";
  subtype Dose_Equivalent         is Item with Dynamic_Predicate => has_Dimension (Dose_Equivalent        , "Sv"      ), Predicate_Failure => raise Unit_Error with "Dose_Equivalent";

  -- Operations

  function "abs" (Right: Item) return Item;

  function "+" (Right: Item) return Item;
  function "-" (Right: Item) return Item;

  function "+" (Left, Right: Item) return Item with Pre => Same_Dimension (Left, Right) or else raise Unit_Error;
  function "-" (Left, Right: Item) return Item with Pre => Same_Dimension (Left, Right) or else raise Unit_Error;

  function "*" (Left, Right: Item) return Item;
  function "/" (Left, Right: Item) return Item;

  function "*" (Left: Item     ; Right: Real'Base) return Item;
  function "*" (Left: Real'Base; Right: Item     ) return Item;
  function "/" (Left: Item     ; Right: Real'Base) return Item;
  function "/" (Left: Real'Base; Right: Item     ) return Item;

  function "**" (Base: Item         ; Exponent: Whole    ) return Item;
  function "**" (Base: Item         ; Exponent: Rational ) return Item;
  function "**" (Base: Dimensionless; Exponent: Real'Base) return Dimensionless;

  function "<"  (Left, Right: Item) return Boolean with Pre => Same_Dimension (Left, Right) or else raise Unit_Error;
  function "<=" (Left, Right: Item) return Boolean with Pre => Same_Dimension (Left, Right) or else raise Unit_Error;
  function ">=" (Left, Right: Item) return Boolean with Pre => Same_Dimension (Left, Right) or else raise Unit_Error;
  function ">"  (Left, Right: Item) return Boolean with Pre => Same_Dimension (Left, Right) or else raise Unit_Error;

  -- Unit strings

  function "*" (Left: Real'Base; Right: String) return Item;  -- 1.0*"km"
  function "/" (Left: Real'Base; Right: String) return Item;  -- 1.0/"s"

  Illegal_Unit: exception;

  -- Mathematics

  Argument_Error: exception renames Ada.Numerics.Argument_Error;

  Pi: constant := Ada.Numerics.Pi;
  e : constant := Ada.Numerics.e;

  function Sqrt (X: Item) return Item;  -- X**(1/2)
  function Cbrt (X: Item) return Item;  -- X**(1/3)

  function Log (X      : Dimensionless) return Dimensionless;
  function Log (X, Base: Dimensionless) return Dimensionless;
  function Exp (X      : Dimensionless) return Dimensionless;

  function Sin (X       : Angle) return Dimensionless;
  function Sin (X, Cycle: Item ) return Dimensionless with Pre => Same_Dimension (X, Cycle) or else raise Unit_Error;
  function Cos (X       : Angle) return Dimensionless;
  function Cos (X, Cycle: Item ) return Dimensionless with Pre => Same_Dimension (X, Cycle) or else raise Unit_Error;
  function Tan (X       : Angle) return Dimensionless;
  function Tan (X, Cycle: Item ) return Dimensionless with Pre => Same_Dimension (X, Cycle) or else raise Unit_Error;
  function Cot (X       : Angle) return Dimensionless;
  function Cot (X, Cycle: Item ) return Dimensionless with Pre => Same_Dimension (X, Cycle) or else raise Unit_Error;

  function Arcsin (X: Dimensionless             ) return Angle;
  function Arcsin (X: Dimensionless; Cycle: Item) return Item;
  function Arccos (X: Dimensionless             ) return Angle;
  function Arccos (X: Dimensionless; Cycle: Item) return Item;
  function Arctan (Y    : Item;
                   X    : Item := One) return Angle with Pre => Same_Dimension (Y, X) or else raise Unit_Error;
  function Arctan (Y    : Item;
                   X    : Item := One;
                   Cycle: Item       ) return Item  with Pre => Same_Dimension (Y, X) or else raise Unit_Error;
  function Arccot (X    : Item;
                   Y    : Item := One) return Angle with Pre => Same_Dimension (X, Y) or else raise Unit_Error;
  function Arccot (X    : Item;
                   Y    : Item := One;
                   Cycle: Item       ) return Item  with Pre => Same_Dimension (X, Y) or else raise Unit_Error;

  function Sinh    (X: Dimensionless) return Dimensionless;
  function Cosh    (X: Dimensionless) return Dimensionless;
  function Tanh    (X: Dimensionless) return Dimensionless;
  function Coth    (X: Dimensionless) return Dimensionless;
  function Arcsinh (X: Dimensionless) return Dimensionless;
  function Arccosh (X: Dimensionless) return Dimensionless;
  function Arctanh (X: Dimensionless) return Dimensionless;
  function Arccoth (X: Dimensionless) return Dimensionless;

private

  pragma Inline (Same_Dimension, has_Dimension, Value,
                 "abs", "+", "-", "*", "/", "**",
                 "<", "<=", ">=", ">",
                 Sqrt, Cbrt,
                 Log, Exp,
                 Sin, Cos, Tan, Cot,
                 Arcsin, Arccos, Arctan, Arccot,
                 Sinh, Cosh, Tanh, Coth,
                 Arcsinh, Arccosh, Arctanh, Arccoth);

  function Image (X: Dimension) return String;

  type Item is record
    Unit : Dimension;
    Value: Real'Base;
  end record;

  Zero: constant Item := (uno, 0.0);
  One : constant Item := (uno, 1.0);

  Radian   : Item renames One;             -- rad
  Steradian: Item renames One;             -- sr

  Meter    : constant Item := (m  , 1.0);  -- m
  Kilogram : constant Item := (kg , 1.0);  -- kg
  Second   : constant Item := (s  , 1.0);  -- s
  Ampere   : constant Item := (A  , 1.0);  -- A
  Kelvin   : constant Item := (K  , 1.0);  -- K
  Candela  : constant Item := (cd , 1.0);  -- cd
  Mole     : constant Item := (mol, 1.0);  -- mol

  -- Derived units

  Hertz    : constant Item := (uno/s              , 1.0);  -- Hz = 1/s
  Becquerel:          Item renames Hertz;                  -- Bq = 1/s

  Newton   : constant Item := (kg*m/s**2          , 1.0);  -- N = m*kg/s**2
  Joule    : constant Item := (kg*m**2/s**2       , 1.0);  -- J = Nm = Ws = VC

  Pascal   : constant Item := (kg/(m*s**2)        , 1.0);  -- Pa = N/m**2

  Watt     : constant Item := (kg*m**2/s**3       , 1.0);  -- W = J/s = VA
  Volt     : constant Item := (kg*m**2/(s**3*A)   , 1.0);  -- V = W/A

  Coulomb  : constant Item := (A*s                , 1.0);  -- C = As

  Ohm      : constant Item := (kg*m**2/(s**3*A**2), 1.0);  -- Ohm = V/A = 1/S
  Siemens  : constant Item := (s**3*A**2/(kg*m**2), 1.0);  -- S = 1/Ohm

  Farad    : constant Item := (s**4*A**2/(kg*m**2), 1.0);  -- F = C/V
  Henry    : constant Item := (kg*m**2/(s**2*A**2), 1.0);  -- H = Vs/A
  Weber    : constant Item := (kg*m**2/(s**2*A)   , 1.0);  -- Wb = Vs
  Tesla    : constant Item := (kg/(s**2*A)        , 1.0);  -- T = Wb/m**2

  Gray     : constant Item := (m**2/s**2          , 1.0);  -- Gy = J/kg
  Sievert  :          Item renames Gray;                   -- Sv = Gy
  -- Do not misinterprete: The conversion of Gy to Sv depends on the kind of
  -- particles: Alpha, Beta, Gamma.

  Lumen    : constant Item := (cd                 , 1.0);  -- lm = cd*sr
  Lux      : constant Item := (cd/m**2            , 1.0);  -- lx = lm/m**2

  Katal    : constant Item := (mol/s              , 1.0);  -- kat = mol/s

  package Elementary_Functions is new Ada.Numerics.Generic_Elementary_Functions (Real'Base);
  use     Elementary_Functions;

end Generic_SI;
