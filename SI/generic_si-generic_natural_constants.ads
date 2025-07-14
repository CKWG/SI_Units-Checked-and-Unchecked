------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2002, 2005, 2006, 2018, 2019, 2020
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
-- As a special exception, if other files instantiate generics from this
-- unit, or you link this unit with other files to produce an executable,
-- this unit does not by itself cause the resulting executable to be
-- covered by the GNU General Public License. This exception does not
-- however invalidate any other reasons why the executable file might be
-- covered by the GNU Public License.
--
-- Author's homepage and email address:
--   http://archive.adaic.com/tools/CKWG/CKWG.html
--   Christ-Usch.Grein@T-Online.de
------------------------------------------------------------------------------

generic

package Generic_SI.Generic_Natural_Constants is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   4.1
  -- Date      9 August 2020
  --====================================================================
  -- SI Redefinition became effective on May 20, 2019.
  -- Didn't find a better abbreviation for e_e, normally named just e.
  -- e_e stands for e(elementary).
  -- Change values, if necessary, to the newest measured values.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    0.0  03.09.2002 Only sketched
  --  C.G.    1.0  23.09.2005 Added some important constants
  --  C.G.    1.1  26.02.2006 pragma Pure
  --  C.G.    2.0  23.07.2018 Unit strings
  --  C.G.    2.1  28.09.2018 Unit syntax changed W/m/K => W/(m*K)
  --  C.G.    3.0  02.10.2018 Newest constant values from NIST
  --  C.G.    3.1  20.05.2019 World Metrology Day
  --  C.G.    3.2  07.03.2020 Stefan-Boltzmann and Gas constants
  --  C.G.    4.0  11.05.2020 Parent renamed to Generic_SI
  --  C.G.    4.1  09.08.2020 Name Speed_of_Light added
  --====================================================================

  -- Speed of Light in Vacuum

  Speed_of_Light: constant Speed := 2.997_924_58E+8*"m/s";  -- exact (by definition)
  c: Speed renames Speed_of_Light;

  -- Particles

  Elementary_Charge: constant Charge := 1.602_176_634E-19*"C";  -- exact (by definition)
  e_e: Charge renames Elementary_Charge;

  Electron_Mass: constant Mass := 9.109_382_6E-31 *"kg";
  Proton_Mass  : constant Mass := 1.672_621_71E-27*"kg";

  m_e: Mass renames Electron_Mass;
  m_p: Mass renames Proton_Mass;

  -- Permittivity and Permeability of Vacuum

  Mu_0 : constant Item := 4.0E-7*Pi*"kg*m/C**2";  -- exact; c = 1/Sqrt(Eps_0*Mu_0)
  Eps_0: constant Item := 1.0/(Mu_0*c**2);        -- 8.854_187_817E-12 F/m

  -- Quantum Mechanics

  Planck: constant Item := 6.626_070_15E-34*"J*s";  -- exact (by definition)
  h     :          Item renames Planck;
  h_bar : constant Item := h / (2.0*Pi);

  Alpha: constant Dimensionless := e_e**2/(2.0*Eps_0*h*c);  -- 1/137.035_999_11

  -- Thermodynamics

  Boltzmann: constant Item := 1.380_649E-23*"J/K";     -- exact (by definition)
  Avogadro : constant Item := 6.022_140_76E+23/"mol";  -- exact (by definition)

  k_B: Item renames Boltzmann;
  N_A: Item renames Avogadro;

  Gas_Constant: constant Item := N_A * k_B;  -- exact; 8.314 J/(mol*K)

  -- Black body radiation

  Stefan_Boltzmann: constant Item := (2.0*Pi**5/15.0)*(k_B/h)**3*(k_B/c**2);  -- careful with underflow
  Sigma           :          Item renames Stefan_Boltzmann;  -- exact; 5.670E-8*W/(m**2*K**4)

  -- Gravity

  Gravity: constant Item := 6.674_2E-11*"m**3/(kg*s**2)";

end Generic_SI.Generic_Natural_Constants;
