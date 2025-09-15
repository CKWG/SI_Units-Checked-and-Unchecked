------------------------------------------------------------------------------
-- Checked and Unchecked Computation with SI Units
-- Copyright (C) 2025 Christoph Karl Walter Grein
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

separate (Generic_SI.Generic_Symbols.Construct)
package body Unit_FSM is

  --====================================================================
  -- Author    Christoph Grein
  -- Version   1.1
  -- Date      15 September 2025
  --====================================================================
  -- Syntax:
  --
  -- external_item ::= value dimension   -- see Note below
  -- dimension     ::= * quotient
  --                 | / divisor
  -- quotient      ::= product [ / divisor]
  -- divisor       ::= unit_factor
  --                 | ( product )
  -- product       ::= unit_factor {* unit_factor}
  -- unit_factor   ::= identifier [** exponent]
  -- identifier    ::= letter {letter}
  -- exponent      ::= ( [sign] rational )
  --                 | numeral
  -- sign          ::= + | -
  -- rational      ::= numeral [ / numeral]
  -- numeral       ::= digit {digit}
  --
  -- internal_item   ::= value * quotient_string
  --                   | value / product_string
  -- quotient_string ::= ""
  --                   | "quotient"
  -- product_string  ::= "product"
  --
  -- A letter is a character that is in one of the ranges 'A'..'Z' or
  -- 'a'..'z' or 'µ'.
  -- The identifier must be interpretable as [prefix] symbol.
  --
  -- Note: The very first production has obligatory dimension in this
  --       FSM, dimensionless items are constructed separately.
  --
  -- The syntax interpretation in case of error may be questionable.
  -- The important fact is that any errors are found.
  --====================================================================
  -- History
  -- Author Version   Date    Reason for change
  --  C.G.    0.0  22.07.2025 Raw design
  --  C.G.    0.1  27.07.2025 FSM structure works
  --  C.G.    0.2  03.08.2025 Integrated in SI hierarchy;
  --                          works without exponents
  --  C.G.    0.3  11.08.2025 Works for unit strings
  --  C.G.    0.4  18.08.2025 Replaced Program_Error with "wrong unit
  --                          begin" by Illegal_Unit
  --  C.G.    1.0  22.08.2025 No further changes
  --  C.G.    1.1  15.09.2025 Allow µ
  --====================================================================

  pragma Optimize (Time);

  -- Result is a product of Factors, Factor the Identifier**Exponent.
  Factor: Item := One;  -- could be replaced by Result

  package Worker is

    type Class is ('*', '/', '(', Sign, ')', Letter, Digit, EoT);

    function Next_Character return Class         with Inline;

    procedure Set_Denominator                    with Inline;

    procedure Take_Letter                        with Inline;

    procedure Evaluate_Symbol                    with Inline;
    procedure No_Exponent                        with Inline;

    procedure Signed_Exponent                    with Inline;
    procedure Take_Digit                         with Inline;
    procedure Evaluate_Whole (Fraction: Boolean) with Inline;
    procedure Evaluate_Simple_Exponent           with Inline;
    procedure Evaluate_Exponent                  with Inline;

    procedure Create_Result                      with Inline;

  end Worker;
  package body Worker is separate;

  ------------------------------------------------------------------------------

  procedure Run is

    type Denominator_Kind is (None, Present, Parenthesized);
    Denominator: Denominator_Kind := None;

    No_Letter: Boolean := True;  -- Symbol must at least have one letter

    type Exponent_Kind is (Parenthesized, Fraction, Neither);
    Exponent: Exponent_Kind := Neither;

    use Worker;

  begin

    case Next_Character is                                  -- starts with one of those
      when '*'    => goto Symbol;
      when '/'    => Denominator := Present;                -- Divisor must follow
                     Set_Denominator;
                     goto Divisor;
      when others => raise Illegal_Unit with "wrong unit begin";
    end case;

    <<Symbol>>

    case Next_Character is
      when Letter => Take_Letter;
                     No_Letter := False;                    -- at least one letter found
                     goto Symbol;                           -- next Letter
      when '*'    => if No_Letter then
                       raise Illegal_Unit with "* no letter in symbol";
                     end if;
                     Evaluate_Symbol;                       -- Symbol ends
                     No_Letter := True;                     -- initialize for next Symbol
                     goto Exponent_or_Next_Symbol;          -- Exponent or next Symbol
      when '/'    => if No_Letter then
                       raise Illegal_Unit with "/ no letter in symbol";
                     end if;
                     Evaluate_Symbol;                       -- Symbol ends
                     No_Exponent;
                     No_Letter := True;
                     if Denominator /= None then            -- we are already in the Divisor
                       raise Illegal_Unit with "second /";
                     end if;
                     Denominator := Present;
                     Set_Denominator;
                     goto Divisor;
      when ')'    => if Denominator /= Parenthesized then
                       raise Illegal_Unit with "spurious ) in denominator";
                     end if;
                     Evaluate_Symbol;
                     No_Exponent;
                     Create_Result;
                     goto End_Check;                        -- Divisor ends
      when EoT    => if Denominator = Parenthesized then
                       raise Illegal_Unit with ") missing";
                     elsif No_Letter then
                       raise Illegal_Unit with "EoT no letter in symbol";
                     end if;
                     Evaluate_Symbol;                       -- Symbol ends: Identifier
                     No_Exponent;                           -- Factor = Identifier
                     Create_Result;                         -- Result := Result * Identifier
                     return;
      when others => raise Illegal_Unit with "syntax error in symbol";                    --
    end case;

    <<Exponent_or_Next_Symbol>>

    case Next_Character is
      when '*'    => goto in_Exponent;
      when Letter => if Denominator = Present then
                       raise Illegal_Unit with "( missing";
                     end if;
                     No_Exponent;                           -- construct Ident from Unit
                     No_Letter := False;                    -- at least one Letter found
                     Take_Letter;                           -- next Symbol begins
                     goto Symbol;
      when others => raise Illegal_Unit with "syntax error after symbol";
    end case;

    -- Exponent ----------------------------------------------------------------

    <<in_Exponent>>

    case Next_Character is                                  -- starts with one of those
      when Digit  => Take_Digit;                            -- first Digit of simple Exponent Sym**9
                     Exponent := Neither;
                     goto Number;
      when '('    => Exponent := Parenthesized;             -- parenthesized Exponent Sym**(
      when others => raise Illegal_Unit with "Exponent? digit or ( expected";
    end case;

    case Next_Character is
      when Sign   => Signed_Exponent;                       -- parenthesized Exponent **(-
                     goto Number;
      when Digit  => Take_Digit;                            -- first digit Sym**(9
                     goto Number;
      when others => raise Illegal_Unit with "sign or digit expected";
    end case;

    <<Number>>                                              -- Sym**Digit  or  Sym**(Sign  or  Sym**(Digit

    case Next_Character is
      when Digit  => Take_Digit;                            -- first Digit after Sign resp. later Digits
                     goto Number;
      when ')'    => if Denominator = None and
                        Exponent = Neither then             -- Sym**Exp)
                       raise Illegal_Unit with "spurious ) in exponent";
                     end if;
                     if Exponent = Fraction then
                       Evaluate_Exponent;                   -- Sym**(N/D)
                       Exponent := Neither;
                     else
                       Evaluate_Whole (Fraction => False);  -- /(Sym**Exp)  or  Sym**(Exp)  or  /(Sym**(Exp)
                       Evaluate_Simple_Exponent;
                       if Exponent = Parenthesized then
                         Exponent := Neither;               -- Sym**(Exp)  or  /(Sym**(Exp)
                       else
                         Denominator := None;               -- /(Sym**Exp)
                       end if;
                     end if;
                     goto After_Exponent;                   -- next: ) | * | / | EoT
      when '*'    => if Exponent = Parenthesized then
                       raise Illegal_Unit with "* in exponent";
                     elsif Denominator = Present then
                       raise Illegal_Unit with "missing (";
                     end if;
                     Evaluate_Whole (Fraction => False);    -- Sym**Exp*
                     Evaluate_Simple_Exponent;
                     goto Symbol;                           -- next: Letter
      when '/'    => if Exponent = Parenthesized then
                       Evaluate_Whole (Fraction => True);   -- Sym**(N/
                       Exponent := Fraction;
                       goto Number;                         -- next: Digit
                     elsif Denominator /= None then
                       raise Illegal_Unit                   -- .../Sym**Exp/
                         with "second / after exponent";
                     else
                       Evaluate_Whole (Fraction => False);  -- Sym**Exp/
                       Evaluate_Simple_Exponent;
                       Denominator := Present;
                       Set_Denominator;
                       goto Divisor;                        -- next: Letter | (
                     end if;
      when EoT    => if Exponent /= Neither then            -- Sym**(Exp/
                       raise Illegal_Unit with "early end of exponent";
                     elsif Denominator = Parenthesized then
                       raise Illegal_Unit with "missing )";
                     end if;
                     Evaluate_Simple_Exponent;              -- Sym**Exp
                     Create_Result;
                     return;                                -- next: nothing
      when others => raise Illegal_Unit with "unexpected character in number";
    end case;

    <<After_Exponent>>                                      -- Exponent is closed

    case Next_Character is
      when ')'    => if Denominator /= Parenthesized then
                       raise Illegal_Unit with "spurious ) after exponent";
                     end if;
                     Denominator := None;                   -- denominator is closed
                     Create_Result;
                     goto End_Check;                        -- Divisor ends
      when '*'    => No_Letter := True;
                     goto Symbol;
      when '/'    => if Denominator /= None then
                       raise Illegal_Unit with "second / after (exponent)";
                     end if;
                     Denominator := Present;
                     Set_Denominator;
                     No_Letter := True;
                     goto Divisor;
      when EoT    => if Denominator = Parenthesized then
                       raise Illegal_Unit with "missing ) after (exponent)";
                     end if;
                     Create_Result;
                     return;
      when others => raise Illegal_Unit                     -- every other character
                       with "syntax error after exponent";  -- allowed in syntax
    end case;

    -- Exponent end ------------------------------------------------------------

    <<Divisor>>

    case Next_Character is
      when Letter => Take_Letter;                           -- m/s
                     No_Letter := False;                    -- at least one Letter found
                     goto Symbol;
      when '('    => Denominator := Parenthesized;          -- m/(
                     goto Symbol;
      when others => raise Illegal_Unit with "divisor missing";
    end case;

    -- End check ---------------------------------------------------------------
    -- Correct syntax ends after closing the divisor.
    -- # Any character that is part of the syntax is considered a syntax error,
    --   a misprint.
    --   Example: "1.0*m/(s)*kg" or "1.0*m/s*kg" instead of "1.0*kg*m/s".
    -- # Any other character is OK.
    --   Example: A full stop or comma after the unit "1.0*m/s.".

    <<End_Check>>

    case Next_Character is
      when '*' | '/' | Letter | '(' | Sign | Digit | ')' =>
        raise Illegal_Unit with "Spurious character after divisor";
      when others => return;
    end case;

  end Run;

end Unit_FSM;
