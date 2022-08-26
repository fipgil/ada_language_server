------------------------------------------------------------------------------
--                         Language Server Protocol                         --
--                                                                          --
--                       Copyright (C) 2022, AdaCore                        --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

with Ada.Containers.Ordered_Maps;

with VSS.Strings;

with LSP_Gen.Entities;

package LSP_Gen.Structures is

   type Or_Mapping_Kind is
     (Type_Union,
      Type_Or_Null,
      Array_Or_Null,
      Type_Class,
      Type_Or_Array,
      Type_Or_Something,
      Two_Types,
      Two_Literals,
      String_Or_Tuple,
      String_Or_Something,
      Boolean_Or_Something,
      Boolean_Or_Class,
      Enumeration,  --  set of stringLiteral
      Option_Combination,
      Boolean_Or_Any,
      Unknown_Mapping);

   type Or_Mapping (Kind : Or_Mapping_Kind := Or_Mapping_Kind'First) is record
      case Kind is
         when Type_Or_Null
            | Type_Class
            | Boolean_Or_Class
            | Option_Combination
            | String_Or_Tuple
            | String_Or_Something
            | Boolean_Or_Something =>

            Tipe : LSP_Gen.Entities.AType;
         when Two_Literals
            | Two_Types
            | Type_Or_Something =>
            First, Second : LSP_Gen.Entities.AType;
         when Type_Or_Array
            | Array_Or_Null =>

            Array_Type    : LSP_Gen.Entities.AType;
         when Enumeration | Type_Union =>
            Items : LSP_Gen.Entities.AType_Vector;
         when Boolean_Or_Any
            | Unknown_Mapping =>

            null;
      end case;
   end record;

   function Get_Or_Mapping
     (Items : LSP_Gen.Entities.AType_Vector) return Or_Mapping;

   function Get_Variant
     (Item  : LSP_Gen.Entities.AType;
      Index : Positive) return VSS.Strings.Virtual_String;
   --  Return variant name for given `or` type item

   type Type_Info is record
      Name         : VSS.Strings.Virtual_String;
      Definition   : LSP_Gen.Entities.Structure;
      Is_Tagged    : Boolean := False;  --  used in `extends`
      Is_Mixin     : Boolean := False;
   end record;

   package Type_Maps is new Ada.Containers.Ordered_Maps
     (VSS.Strings.Virtual_String,
      Type_Info,
      VSS.Strings."<");

   Types : Type_Maps.Map;

   procedure Write_Types (Model : LSP_Gen.Entities.MetaModel);
   --  Write type declarations for each type

end LSP_Gen.Structures;
