------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2019, 2020, 2022, 2025 Christoph Karl Walter Grein
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
with Ada.Strings.Fixed;
with Rational_Arithmetics.Strings;

package body Generic_SI.Generic_Symbols is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   2.5
  -- Date      11 April 2025
  --====================================================================
  -- List of all unit symbols.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    1.0  15.02.2019 subpackage Symbols inserted
  --  C.G.    2.0  13.05.2020 Moved from Generic_Strings
  --  C.G.    2.1  23.07.2020 Symbol_Info with discriminant, component
  --                          Prefix removed
  --  C.G.    2.2  09.08.2020 eV added
  --  C.G.    2.3  08.12.2022 prefixes QqRr added;
  --                          Work-around for [T520-013 public] removed
  --  C.G.    2.4  27.02.2025 deka -> deca (change not visible outside)
  --  C.G.    2.5  11.04.2025 ha added
  --====================================================================

  subtype Symbol_Length is Positive range 1 .. 3;

  type Symbol_Info (Length: Symbol_Length := 1) is record
    Symbol: String (1 .. Length);
    Value : Item;
  end record;

  type Symbol_Enum is (A, Bq, C, F, Gy, H, Hz, J, K, L, N, Om, Pa, Si, Sv, T, V, W, Wb,
                       cd, eV, g, ha, hor, kat, lit, lm, lx, m, min, mol, rad, s, sr);
  -- Make case statement in Evaluate readable

  Symbol_List: constant array (Symbol_Enum) of Symbol_Info :=
    (A   => (Symbol => "A"  , Length => 1, Value => Ampere                 ),
     Bq  => (Symbol => "Bq" , Length => 2, Value => Becquerel              ),
     C   => (Symbol => "C"  , Length => 1, Value => Coulomb                ),
     F   => (Symbol => "F"  , Length => 1, Value => Farad                  ),
     Gy  => (Symbol => "Gy" , Length => 2, Value => Gray                   ),
     H   => (Symbol => "H"  , Length => 1, Value => Henry                  ),
     Hz  => (Symbol => "Hz" , Length => 2, Value => Hertz                  ),
     J   => (Symbol => "J"  , Length => 1, Value => Joule                  ),
     K   => (Symbol => "K"  , Length => 1, Value => Kelvin                 ),
     L   => (Symbol => "L"  , Length => 1, Value => 1.0E-3*Meter**3        ),
     N   => (Symbol => "N"  , Length => 1, Value => Newton                 ),
     Om  => (Symbol => "Ohm", Length => 3, Value => Ohm                    ),  -- replaces Greek Capital Omega
     Pa  => (Symbol => "Pa" , Length => 2, Value => Pascal                 ),
     Si  => (Symbol => "S"  , Length => 1, Value => Siemens                ),
     Sv  => (Symbol => "Sv" , Length => 2, Value => Sievert                ),
     T   => (Symbol => "T"  , Length => 1, Value => Tesla                  ),
     V   => (Symbol => "V"  , Length => 1, Value => Volt                   ),
     W   => (Symbol => "W"  , Length => 1, Value => Watt                   ),
     Wb  => (Symbol => "Wb" , Length => 2, Value => Weber                  ),
     --                  "alphabetically" ordered
     cd  => (Symbol => "cd" , Length => 2, Value => Candela                ),
     eV  => (Symbol => "eV" , Length => 2, Value => 1.602_176_634E-19*Joule),  -- the same value as in Generic_Natural_Constants
     g   => (Symbol => "g"  , Length => 1, Value => 1.0E-3*Kilogram        ),
     ha  => (Symbol => "ha" , Length => 2, Value => 1.0E+4*Meter**2        ),  -- unit for field survey, no prefix
     hor => (Symbol => "h"  , Length => 1, Value => 60.0**2*Second         ),  -- no prefix
     kat => (Symbol => "kat", Length => 3, Value => Katal                  ),
     lit => (Symbol => "l"  , Length => 1, Value => 1.0E-3*Meter**3        ),
     lm  => (Symbol => "lm" , Length => 2, Value => Lumen                  ),
     lx  => (Symbol => "lx" , Length => 2, Value => Lux                    ),
     m   => (Symbol => "m"  , Length => 1, Value => Meter                  ),
     min => (Symbol => "min", Length => 3, Value => 60.0*Second            ),  -- no prefix
     mol => (Symbol => "mol", Length => 3, Value => Mole                   ),
     rad => (Symbol => "rad", Length => 3, Value => Radian                 ),
     s   => (Symbol => "s"  , Length => 1, Value => Second                 ),
     sr  => (Symbol => "sr" , Length => 2, Value => Steradian              ));

  -- Prefixes
  -- Any base unit may be prefixed with one of the constants below.
  -- Only the Kilogram is an exception: It is illegal to write
  -- Mkg (Megakilogram), the correct designation is Gg (Gigagram).

  quecto: constant := 1.0E-30;  -- q
  ronto : constant := 1.0E-27;  -- r
  yocto : constant := 1.0E-24;  -- y
  zepto : constant := 1.0E-21;  -- z
  atto  : constant := 1.0E-18;  -- a
  femto : constant := 1.0E-15;  -- f
  pico  : constant := 1.0E-12;  -- p
  nano  : constant := 1.0E-09;  -- n
  micro : constant := 1.0E-06;  -- µ replaced by u
  milli : constant := 1.0E-03;  -- m
  centi : constant := 1.0E-02;  -- c
  deci  : constant := 1.0E-01;  -- d
  deca  : constant := 1.0E+01;  -- da
  hecto : constant := 1.0E+02;  -- h
  kilo  : constant := 1.0E+03;  -- k
  mega  : constant := 1.0E+06;  -- M
  giga  : constant := 1.0E+09;  -- G
  tera  : constant := 1.0E+12;  -- T
  peta  : constant := 1.0E+15;  -- P
  exa   : constant := 1.0E+18;  -- E
  zetta : constant := 1.0E+21;  -- Z
  yotta : constant := 1.0E+24;  -- Y
  ronna : constant := 1.0E+27;  -- R
  quetta: constant := 1.0E+30;  -- Q

  Symbol_Characters: Ada.Strings.Maps.Character_Set :=     -- in fact a constant, completed below
    Ada.Strings.Maps.To_Set ("qryzafpnumcdahkMGTPEZYRQ");  -- the prefixes

  function Legal_Characters return Ada.Strings.Maps.Character_Set is
    use Ada.Strings.Maps;
  begin
    return Symbol_Characters;
  end Legal_Characters;

  function Evaluate (Symbol: String) return Item  is separate;

  function Image (X: Dimension) return String is separate;

begin

  declare
    procedure Collect_Symbol_Characters is
      use Ada.Strings.Maps;
    begin
      for S of Symbol_List loop
        Symbol_Characters := Symbol_Characters or To_Set (S.Symbol (1 .. S.Length));
      end loop;
    end Collect_Symbol_Characters;
  begin
    Collect_Symbol_Characters;
  end;

end Generic_SI.Generic_Symbols;
