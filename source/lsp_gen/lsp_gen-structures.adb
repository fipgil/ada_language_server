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

with Ada.Containers.Ordered_Sets;

with LSP_Gen.Puts; use LSP_Gen.Puts;
with VSS.String_Vectors;

package body LSP_Gen.Structures is

   use all type LSP_Gen.Entities.Enum.AType_Variant;
   use type LSP_Gen.Entities.Enum.BaseTypes;
   use type VSS.Strings.Virtual_String;

   package String_Sets is new Ada.Containers.Ordered_Sets
     (VSS.Strings.Virtual_String,
      VSS.Strings."<",
      VSS.Strings."=");

   function Predefined_Equal (L, R : LSP_Gen.Entities.AType) return Boolean
     renames LSP_Gen.Entities."=";

   function "=" (Left, Right : LSP_Gen.Entities.AType) return Boolean;

   procedure Find_Optional_And_Arrays;
   procedure Write_Mixins (Done : in out String_Sets.Set);
   procedure Write_Mixin
     (Item : LSP_Gen.Entities.Structure;
      Done : in out String_Sets.Set);
   procedure Write_Structure
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);
   procedure Write_Type
     (Name     : VSS.Strings.Virtual_String;
      Item     : LSP_Gen.Entities.AType;
      Fallback : VSS.Strings.Virtual_String);
   procedure Write_Type_Alias
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);
   procedure Write_Enum
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);
   procedure Write_Properties
     (List        : LSP_Gen.Entities.Property_Vector;
      Is_Optional : Boolean := False;
      Enclosing   : VSS.Strings.Virtual_String);
   --  Force all properties to be optional if Is_Optional
   procedure Write_Property
     (Item        : LSP_Gen.Entities.Property;
      Is_Optional : Boolean := False;
      Enclosing   : VSS.Strings.Virtual_String);
   --  Force property to be optional if Is_Optional
   procedure Write_Type_Name
     (Item        : LSP_Gen.Entities.AType;
      Is_Optional : Boolean);
   procedure Write_Or_Null_Type
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector);
   procedure Write_Two_Types
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector;
      Fallback_2 : VSS.Strings.Virtual_String := "");
   procedure Write_Two_Literals
     (Name   : VSS.Strings.Virtual_String;
      Base   : LSP_Gen.Entities.AType;
      Extend : LSP_Gen.Entities.AType);
   procedure Write_Optional_Type (Name : VSS.Strings.Virtual_String);
   procedure Write_Vector_Type
     (Name : VSS.Strings.Virtual_String;
      Item : VSS.Strings.Virtual_String);
   procedure Write_Enumeration
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector);
   procedure Write_Union
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector);
   procedure Write_Class_Type
     (Name : VSS.Strings.Virtual_String;
      Item : LSP_Gen.Entities.AType);
   procedure Write_Boolean_Or_Class
     (Name : VSS.Strings.Virtual_String;
      Item : LSP_Gen.Entities.AType);
   procedure Write_Private_Part;
   procedure Emit_Dependence
     (Item      : LSP_Gen.Entities.AType;
      Skip      : VSS.Strings.Virtual_String;
      Done      : in out String_Sets.Set;
      Fallback  : VSS.Strings.Virtual_String := "";
      With_Type : Boolean := False;
      Optional  : Boolean := False);
   procedure Emit_Dependence
     (Item : LSP_Gen.Entities.AType_Vector;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);

   procedure Emit_Dependence
     (Item      : LSP_Gen.Entities.Property_Vector;
      Skip      : VSS.Strings.Virtual_String;
      Done      : in out String_Sets.Set;
      Optional  : Boolean := False;
      Enclosing : VSS.Strings.Virtual_String);

   function Get_Or_Mapping
     (Items : LSP_Gen.Entities.AType_Vector) return Or_Mapping;

   function Is_Extents
     (Child, Parent : LSP_Gen.Entities.AType) return Boolean;
   --  Check if Child structure extends Parent

   function Has_Kind (Item : LSP_Gen.Entities.AType) return Boolean;
   --  the type has `kind` stringLiteral property

   function Short_Name (Item : LSP_Gen.Entities.AType)
     return VSS.Strings.Virtual_String;
   --  Return simple defining name for a type

   function Short_Name (Item : LSP_Gen.Entities.MapKeyType)
     return VSS.Strings.Virtual_String;

   function Short_Name
     (Item     : LSP_Gen.Entities.AType;
      Fallback : VSS.Strings.Virtual_String)
        return VSS.Strings.Virtual_String;
   --  Return simple defining name for a type or Fallback

   function Base_Index (List : LSP_Gen.Entities.AType_Vector) return Positive;
   --  Return index of base type from given `extends` list.

   Base_Short_Name : constant array (LSP_Gen.Entities.Enum.BaseTypes) of
     VSS.Strings.Virtual_String :=
       (LSP_Gen.Entities.Enum.Uri |
        LSP_Gen.Entities.Enum.DocumentUri => "DocumentUri",
        LSP_Gen.Entities.Enum.integer     => "Integer",
        LSP_Gen.Entities.Enum.uinteger    => "Natural",
        LSP_Gen.Entities.Enum.decimal     => "Float",
        LSP_Gen.Entities.Enum.RegExp |
        LSP_Gen.Entities.Enum.string      => "Virtual_String",
        LSP_Gen.Entities.Enum.a_boolean   => "Boolean",
        LSP_Gen.Entities.Enum.a_null      => "???");

   Base_Full_Name : constant array (LSP_Gen.Entities.Enum.BaseTypes) of
     VSS.Strings.Virtual_String :=
       (LSP_Gen.Entities.Enum.Uri |
        LSP_Gen.Entities.Enum.DocumentUri => "LSP.Structures.DocumentUri",
        LSP_Gen.Entities.Enum.integer     => "Standard.Integer",
        LSP_Gen.Entities.Enum.uinteger    => "Natural",
        LSP_Gen.Entities.Enum.decimal     => "Float",
        LSP_Gen.Entities.Enum.RegExp |
        LSP_Gen.Entities.Enum.string      => "LSP.Structures.Virtual_String",
        LSP_Gen.Entities.Enum.a_boolean   => "Standard.Boolean",
        LSP_Gen.Entities.Enum.a_null      => "???");

   package Constants is
      Set    : constant VSS.Strings.Virtual_String := "_Set";
      Vector : constant VSS.Strings.Virtual_String := "_Vector";
   end Constants;

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : LSP_Gen.Entities.AType) return Boolean is
      use type LSP_Gen.Entities.MapKeyType;

      function "=" (L, R : LSP_Gen.Entities.AType_Vector) return Boolean;
      function "=" (L, R : LSP_Gen.Entities.Property_Vector) return Boolean;

      ---------
      -- "=" --
      ---------

      function "=" (L, R : LSP_Gen.Entities.AType_Vector) return Boolean is
      begin
         if L.Length /= R.Length then
            return False;
         end if;

         for J in 1 .. L.Length loop
            if L (J) /= R (J) then
               return False;
            end if;
         end loop;

         return True;
      end "=";

      ---------
      -- "=" --
      ---------

      function "=" (L, R : LSP_Gen.Entities.Property_Vector) return Boolean is
      begin
         if L.Length /= R.Length then
            return False;
         end if;

         for J in 1 .. L.Length loop
            if L (J).name /= R (J).name or else
              L (J).optional /= R (J).optional or else
              L (J).a_type /= R (J).a_type
            then
               return False;
            end if;
         end loop;

         return True;
      end "=";
   begin
      if Left.Union.Kind /= Right.Union.Kind then
         return False;
      end if;

      case Left.Union.Kind is
         when base
            | reference
            | stringLiteral
            | integerLiteral
            | booleanLiteral =>

            return Predefined_Equal (Left, Right);
         when an_array =>
            return Left.Union.an_array.element.Value =
              Right.Union.an_array.element.Value;
         when map =>
            return Left.Union.map.key = Right.Union.map.key and then
              Left.Union.map.value.Value = Right.Union.map.value.Value;
         when an_and =>
            return Left.Union.an_and.items = Right.Union.an_and.items;
         when a_or =>
            return Left.Union.a_or.items = Right.Union.a_or.items;
         when tuple =>
            return Left.Union.tuple.items = Right.Union.tuple.items;
         when literal =>
            return Left.Union.literal.value.properties =
              Right.Union.literal.value.properties;
      end case;
   end "=";

   ----------------
   -- Base_Index --
   ----------------

   function Base_Index
     (List : LSP_Gen.Entities.AType_Vector) return Positive
   is
      First : constant VSS.Strings.Virtual_String :=
        List (1).Union.reference.name;
   begin
      pragma Assert (List.Length in 1 .. 2);

      if List.Length = 1 or First /= "TextDocumentRegistrationOptions" then
         return 1;
      else
         return 2;
      end if;
   end Base_Index;

   ---------------------
   -- Emit_Dependence --
   ---------------------

   procedure Emit_Dependence
     (Item      : LSP_Gen.Entities.AType;
      Skip      : VSS.Strings.Virtual_String;
      Done      : in out String_Sets.Set;
      Fallback  : VSS.Strings.Virtual_String := "";
      With_Type : Boolean := False;
      Optional  : Boolean := False)
   is
      Need_Type : constant Boolean := With_Type or
       Item.Union.Kind in an_array | map | a_or | literal | tuple;

      Name      : constant VSS.Strings.Virtual_String :=
        (if With_Type then Fallback
         else Short_Name (Item, Fallback));
   begin
      case Item.Union.Kind is
         when base | stringLiteral =>

            null;

         when reference =>
            if Skip /= Item.Union.reference.name then
               if Types.Contains (Item.Union.reference.name) then
                  Write_Structure (Item.Union.reference.name, Done);
               elsif Aliases.Contains (Item.Union.reference.name) then
                  Write_Type_Alias (Item.Union.reference.name, Done);
               elsif Enums.Contains (Item.Union.reference.name) then
                  Write_Enum (Item.Union.reference.name, Done);
               end if;
            end if;

         when an_array =>
            if Fallback /= "LSPArray" then
               Emit_Dependence
                 (Item.Union.an_array.element.Value,
                  Skip,
                  Done,
                  Fallback & "_Item");
            end if;

         when a_or =>
            if Fallback /= "LSPAny" then
               declare
                  Map : constant Or_Mapping :=
                    Get_Or_Mapping (Item.Union.a_or.items);
               begin
                  case Map.Kind is
                     when Boolean_Or_Something =>
                        Emit_Dependence
                          (Map.Tipe, Skip, Done, Fallback & "_Literal");

                     when Boolean_Or_Any =>
                        null;

                     when Boolean_Or_Class =>
                        declare
                           Class_Name : constant VSS.Strings.Virtual_String :=
                             Short_Name (Map.Tipe) & "_Access";
                        begin
                           Emit_Dependence (Item.Union.a_or.items, Skip, Done);

                           if not Done.Contains (Class_Name) then
                              Write_Class_Type (Class_Name, Map.Tipe);
                              New_Line;
                              Done.Insert (Class_Name);
                           end if;
                        end;

                     when Option_Combination =>
                        Emit_Dependence
                          (Map.Tipe.Union.literal.value.properties,
                           Skip,
                           Done,
                           Optional => True,
                           Enclosing => Name);

                     when Type_Or_Something =>
                        Emit_Dependence (Map.First, Skip, Done);

                        Emit_Dependence
                          (Map.Second, Skip, Done, Fallback);

                     when Two_Literals =>
                        null;

                     when String_Or_Something =>
                        Emit_Dependence
                          (Map.Tipe, Skip, Done, Fallback & "_Literal");

                     when others =>
                        Emit_Dependence (Item.Union.a_or.items, Skip, Done);
                  end case;
               end;
            end if;

         when literal =>
            Emit_Dependence
              (Item.Union.literal.value.properties,
               Skip,
               Done,
               Enclosing => Name);

         when map =>
            Emit_Dependence
              (Item.Union.map.value.Value, Skip, Done, Fallback & "_Item");

         when tuple =>
            Emit_Dependence (Item.Union.tuple.items, Skip, Done);

         when an_and |
              integerLiteral | booleanLiteral =>
            raise Program_Error;
      end case;

      if Need_Type then
         if not Done.Contains (Name) then
            Done.Insert (Name);
            Write_Type (Name, Item, Fallback);

            if not With_Type then
               New_Line;
            end if;
         end if;

         if Optional and
           Item.Union.Kind in literal | a_or and
           not Done.Contains (Name & "_Optional")
         then
            Done.Insert (Name & "_Optional");

            Write_Optional_Type (Name);
         end if;
      end if;
   end Emit_Dependence;

   ---------------------
   -- Emit_Dependence --
   ---------------------

   procedure Emit_Dependence
     (Item : LSP_Gen.Entities.AType_Vector;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set) is
   begin
      for J in 1 .. Item.Length loop
         Emit_Dependence (Item (J), Skip, Done);
      end loop;
   end Emit_Dependence;

   ---------------------
   -- Emit_Dependence --
   ---------------------

   procedure Emit_Dependence
     (Item      : LSP_Gen.Entities.Property_Vector;
      Skip      : VSS.Strings.Virtual_String;
      Done      : in out String_Sets.Set;
      Optional  : Boolean := False;
      Enclosing : VSS.Strings.Virtual_String) is
   begin
      for J in 1 .. Item.Length loop
         Emit_Dependence
           (Item (J).a_type,
            Skip,
            Done,
            Fallback => Item (J).name & "_Of" & Enclosing,
            Optional => Item (J).optional or Optional);
      end loop;
   end Emit_Dependence;

   ------------------------------
   -- Find_Optional_And_Arrays --
   ------------------------------

   procedure Find_Optional_And_Arrays is

      procedure Process (Item : LSP_Gen.Entities.AType);
      procedure Process (Item : LSP_Gen.Entities.AType_Vector);
      procedure Process (Item : LSP_Gen.Entities.Property_Vector);

      -------------
      -- Process --
      -------------

      procedure Process (Item : LSP_Gen.Entities.AType) is
      begin
         case Item.Union.Kind is
            when base | reference | stringLiteral =>
               null;
            when an_array =>
               declare
                  Element : constant LSP_Gen.Entities.AType :=
                    Item.Union.an_array.element.Value;
               begin
                  case Element.Union.Kind is
                     when reference =>
                        if Types.Contains (Element.Union.reference.name) then
                           Types (Element.Union.reference.name).Has_Array :=
                             True;
                        elsif Aliases.Contains
                          (Element.Union.reference.name)
                        then
                           Aliases (Element.Union.reference.name).Has_Array :=
                             True;
                        elsif Enums.Contains
                          (Element.Union.reference.name)
                        then
                           Enums (Element.Union.reference.name).Has_Array :=
                             True;
                        end if;
                     when others =>
                        Process (Element);
                  end case;
               end;

            when a_or =>
               Process (Item.Union.a_or.items);

            when literal =>
               Process (Item.Union.literal.value.properties);

            when map =>
               Process (Item.Union.map.value.Value);

            when tuple =>
               Process (Item.Union.tuple.items);

            when an_and |
                 integerLiteral | booleanLiteral =>
               raise Program_Error;
         end case;
      end Process;

      -------------
      -- Process --
      -------------

      procedure Process (Item : LSP_Gen.Entities.AType_Vector) is
      begin
         for J in 1 .. Item.Length loop
            Process (Item (J));
         end loop;
      end Process;

      -------------
      -- Process --
      -------------

      procedure Process (Item : LSP_Gen.Entities.Property_Vector) is
      begin
         for J in 1 .. Item.Length loop
            declare
               Property : constant LSP_Gen.Entities.Property :=
                 Item (J);
            begin
               if Property.optional and then
                 Property.a_type.Union.Kind = reference
               then
                  if Types.Contains
                    (Property.a_type.Union.reference.name)
                  then
                     Types
                       (Property.a_type.Union.reference.name).Has_Optional
                       := True;
                  elsif Aliases.Contains
                    (Property.a_type.Union.reference.name)
                  then
                     Aliases
                       (Property.a_type.Union.reference.name).Has_Optional
                       := True;
                  elsif Enums.Contains
                    (Property.a_type.Union.reference.name)
                  then
                     Enums
                       (Property.a_type.Union.reference.name).Has_Optional
                       := True;
                  end if;
               end if;

               Process (Property.a_type);
            end;
         end loop;

      end Process;

   begin
      for Item of Types loop
         Process (Item.Definition.extends);
         Process (Item.Definition.mixins);
         Process (Item.Definition.properties);

         for K in 1 .. Item.Definition.mixins.Length loop
            declare
               Mixin : constant LSP_Gen.Entities.AType :=
                 Item.Definition.mixins (K);
            begin
               pragma Assert (Mixin.Union.Kind = reference);

               Types (Mixin.Union.reference.name).Is_Mixin := True;
            end;
         end loop;

         for K in 1 .. Item.Definition.extends.Length loop
            declare
               Base : constant LSP_Gen.Entities.AType :=
                 Item.Definition.extends (K);
            begin
               pragma Assert (Base.Union.Kind = reference);

               Types (Base.Union.reference.name).Is_Tagged := True;
            end;
         end loop;
      end loop;
   end Find_Optional_And_Arrays;

   --------------------
   -- Get_Or_Mapping --
   --------------------

   function Get_Or_Mapping
     (Items : LSP_Gen.Entities.AType_Vector) return Or_Mapping
   is
      function Same_Option
        (Left, Right : LSP_Gen.Entities.AType) return Boolean;
      --  items are the same except `optional` field

      -----------------
      -- Same_Option --
      -----------------

      function Same_Option
        (Left, Right : LSP_Gen.Entities.AType) return Boolean is
      begin
         if Left.Union.Kind /= literal or else
           Right.Union.Kind /= literal or else
           Left.Union.literal.value.properties.Length /=
             Right.Union.literal.value.properties.Length
         then
            return False;
         end if;

         for J in 1 .. Left.Union.literal.value.properties.Length loop
            declare
               L : constant LSP_Gen.Entities.Property :=
                 Left.Union.literal.value.properties (J);
               R : constant LSP_Gen.Entities.Property :=
                 Right.Union.literal.value.properties (J);
            begin
               if L.name /= R.name or else L.a_type /= R.a_type then
                  return False;
               end if;
            end;
         end loop;

         return True;
      end Same_Option;

      Last : constant LSP_Gen.Entities.AType := Items (Items.Length);

      Has_Null : constant Boolean := Last.Union.Kind in base
        and then Last.Union.base.name = LSP_Gen.Entities.Enum.a_null;
   begin
      --  Some base type and `null`
      if Items.Length = 2 and then
        Has_Null and then Items (1).Union.Kind in base | reference
      then
         return (Type_Or_Null, Items (1));
      end if;

      --  Some array type and `null`
      if Items.Length = 2 and then
        Has_Null and then Items (1).Union.Kind in an_array
      then
         return (Array_Or_Null, Items (1));
      end if;

      --  Some base/reference type or an array of it
      if Items.Length = 2 and then
        Items (1).Union.Kind in base | reference and then
        Items (2).Union.Kind in an_array and then
        Items (2).Union.an_array.element.Value = Items (1)
      then
         return (Type_Or_Array, Items (2));
      end if;

      --  Each type extends the first item
      if (for all J in 2 .. Items.Length =>
           Is_Extents (Items (J), Items (1)))
      then
         return (Type_Class, Items (1));
      end if;

      --  Boolean and two types, 3 extends 2
      if Items.Length = 3 and then
        Items (1).Union.Kind = base and then
        Items (1).Union.base.name = LSP_Gen.Entities.Enum.a_boolean and then
        Is_Extents (Items (3), Items (2))
      then
         return (Boolean_Or_Class, Items (2));
      end if;

      --  Each type (except the first) has `kind` stringLiteral property
      if (for all J in 2 .. Items.Length => Has_Kind (Items (J))) then
         return (Type_Union, Items);
      end if;

      --  All items are the same except `optional` field
      if (for all J in 2 .. Items.Length =>
           Same_Option (Items (1), Items (J)))
      then
         return (Option_Combination, Items (1));
      end if;

      --  A union of two types (base or reference)
      if Items.Length = 2 and then
        (for all J in 1 .. Items.Length => Items (J).Union.Kind in
            reference | base)
      then
         return (Two_Types, Items (1), Items (2));
      end if;

      --  A union of types X, Y, array Y
      if Items.Length = 3 and then
        (for all J in 1 .. 2 => Items (J).Union.Kind = reference) and then
        Items (3).Union.Kind = an_array and then
        Items (3).Union.an_array.element.Value = Items (2)
      then
         return (Two_Types, Items (1), Items (3));
      end if;

      --  A union of string and some array
      if Items.Length = 2 and then
        Items (1).Union.Kind = base and then
        Items (1).Union.base.name = LSP_Gen.Entities.Enum.string and then
        Items (2).Union.Kind = an_array
      then
         return (Two_Types, Items (1), Items (2));
      end if;

      --  A union of string and tuple
      if Items.Length = 2 and then
        Items (1).Union.Kind = base and then
        Items (1).Union.base.name = LSP_Gen.Entities.Enum.string and then
        Items (2).Union.Kind = tuple
      then
         return (String_Or_Tuple, Items (2));
      end if;

      --  A union of string and some literal
      if Items.Length = 2 and then
        Items (1).Union.Kind = base and then
        Items (1).Union.base.name = LSP_Gen.Entities.Enum.string and then
        Items (2).Union.Kind = literal
      then
         return (String_Or_Something, Items (2));
      end if;

      --  A union of boolean and an empty object
      if Items.Length = 2 and then
        Items (1).Union.Kind = base and then
        Items (1).Union.base.name = LSP_Gen.Entities.Enum.a_boolean and then
        Items (2).Union.Kind = literal and then
        Items (2).Union.literal.value.properties.Length = 0
      then
         return (Kind => Boolean_Or_Any);
      end if;

      --  A union of boolean and some literal
      if Items.Length = 2 and then
        Items (1).Union.Kind = base and then
        Items (1).Union.base.name = LSP_Gen.Entities.Enum.a_boolean and then
        Items (2).Union.Kind = literal
      then
         return (Boolean_Or_Something, Items (2));
      end if;

      --  All items are stringLiteral
      if (for all J in 1 .. Items.Length =>
            Items (J).Union.Kind = stringLiteral)
      then
         return (Enumeration, Items);
      end if;

      --  A union of `Location` (or `Range`) and some literal
      if Items.Length = 2 and then
        Items (1).Union.Kind = reference and then
        Items (2).Union.Kind = literal
      then
         return (Type_Or_Something, Items (1), Items (2));
      end if;

      --  A union of two literals
      if Items.Length = 2 and then
        (for all J in 1 .. Items.Length => Items (J).Union.Kind = literal)
      then
         if Items (1).Union.literal.value.properties.Length <
           Items (2).Union.literal.value.properties.Length
         then
            return (Two_Literals, Items (1), Items (2));
         else
            return (Two_Literals, Items (2), Items (1));
         end if;
      end if;

      return (Kind => Unknown_Mapping);
   end Get_Or_Mapping;

   --------------
   -- Has_Kind --
   --------------

   function Has_Kind (Item : LSP_Gen.Entities.AType) return Boolean is
   begin
      return Item.Union.Kind = reference
        and then Types.Contains (Item.Union.reference.name)
        and then Types (Item.Union.reference.name).Definition
          .properties.Length > 1
        and then Types (Item.Union.reference.name).Definition
          .properties (1).name = "kind"
        and then Types (Item.Union.reference.name).Definition
          .properties (1).a_type.Union.Kind = stringLiteral;
   end Has_Kind;

   ----------------
   -- Is_Extents --
   ----------------

   function Is_Extents
     (Child, Parent : LSP_Gen.Entities.AType) return Boolean is
   begin
      return Child.Union.Kind = reference
        and then Parent.Union.Kind = reference
        and then Types.Contains (Child.Union.reference.name)
        and then Types
          (Child.Union.reference.name).Definition.extends.Length >= 1
        and then Types (Child.Union.reference.name).Definition.extends
          (Base_Index (Types (Child.Union.reference.name).Definition.extends))
             = Parent;
   end Is_Extents;

   ----------------
   -- Short_Name --
   ----------------

   function Short_Name
     (Item : LSP_Gen.Entities.AType)
        return VSS.Strings.Virtual_String is
   begin
      case Item.Union.Kind is
         when base =>
            return Base_Short_Name (Item.Union.base.name);
         when reference =>
            return Item.Union.reference.name;
         when an_array =>
            declare
               Element : constant VSS.Strings.Virtual_String :=
                 Short_Name (Item.Union.an_array.element.Value);
            begin
               return Element &
                 (if Enums.Contains (Element) then Constants.Set
                  else Constants.Vector);
            end;
         when a_or =>
            declare
               List : constant LSP_Gen.Entities.AType_Vector :=
                 Item.Union.a_or.items;

               Mapping : constant Or_Mapping := Get_Or_Mapping (List);
            begin
               case Mapping.Kind is
                  when Type_Or_Null =>
                     return Short_Name (Mapping.Tipe) & "_Or_Null";
                  when Array_Or_Null =>
                     return Short_Name (Mapping.Array_Type) & "_Or_Null";
                  when Type_Class =>
                     return Short_Name (Mapping.Tipe) & "_Access";
                  when Type_Or_Array =>
                     return Short_Name (Mapping.Array_Type);
                  when Option_Combination =>
                     return Short_Name (Mapping.Tipe);
                  when Two_Types =>
                     return Short_Name (Mapping.First) & "_Or_" &
                       Short_Name (Mapping.Second);
                  when String_Or_Tuple =>
                     return "String_Or_" & Short_Name (Mapping.Tipe);
                  when Boolean_Or_Any =>
                     return "LSPAny";
                  when Type_Or_Something =>
                     return Short_Name (Mapping.First) & "_Or_Something";
                  when Type_Union
                     | Two_Literals
                     | String_Or_Something
                     | Boolean_Or_Something
                     | Boolean_Or_Class
                     | Enumeration
                     | Unknown_Mapping =>

                     raise Program_Error;
               end case;
            end;
         when tuple =>
            return Short_Name (Item.Union.tuple.items (1)) &
              "_Tuple";
         when others =>
            raise Program_Error;
      end case;
   end Short_Name;

   ----------------
   -- Short_Name --
   ----------------

   function Short_Name
     (Item     : LSP_Gen.Entities.AType;
      Fallback : VSS.Strings.Virtual_String)
        return VSS.Strings.Virtual_String is
   begin
      case Item.Union.Kind is
         when base | reference =>
            return Short_Name (Item);

         when an_array =>
            declare
               Element_Fallback : constant VSS.Strings.Virtual_String :=
                 Fallback & "_Item";

               Result : constant VSS.Strings.Virtual_String := Short_Name
                 (Item.Union.an_array.element.Value, Element_Fallback);
            begin
               if Result.Starts_With (Element_Fallback) then
                  return Fallback;
               else
                  return Short_Name (Item);
               end if;
            end;
         when a_or =>
            declare
               List : constant LSP_Gen.Entities.AType_Vector :=
                 Item.Union.a_or.items;

               Mapping : constant Or_Mapping := Get_Or_Mapping (List);
            begin
               case Mapping.Kind is
                  when Type_Union
                     | Two_Literals
                     | String_Or_Something
                     | Type_Or_Something
                     | Boolean_Or_Something
                     | Boolean_Or_Class
                     | Enumeration
                     | Unknown_Mapping =>

                     return Fallback;

                  when Option_Combination =>
                     return Short_Name (Mapping.Tipe, Fallback);

                  when others =>
                     return Short_Name (Item);
               end case;
            end;

         when tuple =>
            return Short_Name (Item);

         when literal =>
            return Fallback;

         when others =>
            return Fallback;
      end case;
   end Short_Name;

   ----------------
   -- Short_Name --
   ----------------

   function Short_Name (Item : LSP_Gen.Entities.MapKeyType)
     return VSS.Strings.Virtual_String is
   begin
      case Item.Union.Kind is
         when LSP_Gen.Entities.Enum.base =>
            case Item.Union.base.name is
               when LSP_Gen.Entities.Enum.DocumentUri =>
                  return "DocumentUri";
               when others =>
                  raise Program_Error;
            end case;

         when LSP_Gen.Entities.Enum.reference =>
            return Item.Union.reference.name;
      end case;
   end Short_Name;

   ----------------------------
   -- Write_Boolean_Or_Class --
   ----------------------------

   procedure Write_Boolean_Or_Class
     (Name : VSS.Strings.Virtual_String;
      Item : LSP_Gen.Entities.AType) is
   begin
      Put ("type ");
      Put (Name);
      Put_Line (" (Is_Boolean : Boolean := True) is record");
      Put_Line ("case Is_Boolean is");
      Put_Line ("   when True =>");
      Put_Line ("Boolean : Standard.Boolean := False;");
      Put_Line ("   when False =>");
      Put ("Object : ");
      Write_Type_Name (Item, False);
      Put_Line ("_Access;");
      Put_Line ("end case;");
      Put_Line ("end record;");
      New_Line;
   end Write_Boolean_Or_Class;

   ----------------------
   -- Write_Class_Type --
   ----------------------

   procedure Write_Class_Type
     (Name : VSS.Strings.Virtual_String;
      Item : LSP_Gen.Entities.AType) is
   begin
      Put ("type ");
      Put (Name);
      Put (" is access all ");
      Put (Short_Name (Item));
      Put_Line ("'Class;");
   end Write_Class_Type;

   ----------------------------------
   -- Write_Document_Symbol_Vector --
   ----------------------------------

   procedure Write_Private_Part is
   begin
      Put_Line
        ("function Length (Self : DocumentSymbol_Vector) return Natural;");
      New_Line;
      Put_Line ("procedure Clear (Self : in out DocumentSymbol_Vector);");
      New_Line;

      Put_Line ("procedure Append (Self : in out DocumentSymbol_Vector;");
      Put_Line ("Value : DocumentSymbol);");
      New_Line;

      Put ("type DocumentSymbol_Variable_Reference");
      Put_Line (" (Element : not null access DocumentSymbol)");
      Put_Line ("is null record with Implicit_Dereference => Element;");
      New_Line;

      Put_Line ("function Get_DocumentSymbol_Variable_Reference");
      Put_Line ("(Self  : aliased in out DocumentSymbol_Vector;");
      Put_Line ("Index : Positive)");
      Put_Line ("return DocumentSymbol_Variable_Reference with Inline;");
      New_Line;

      Put ("type DocumentSymbol_Constant_Reference");
      Put_Line (" (Element : not null access constant DocumentSymbol)");
      Put_Line ("is null record with Implicit_Dereference => Element;");
      New_Line;

      Put_Line ("function Get_DocumentSymbol_Constant_Reference");
      Put_Line ("(Self  : aliased DocumentSymbol_Vector;");
      Put_Line ("Index : Positive)");
      Put_Line ("return DocumentSymbol_Constant_Reference with Inline;");
      New_Line;

      Put_Line
        ("function Is_Set (Self : SelectionRange_Optional) return Boolean;");
      Put ("function Value (Self : SelectionRange_Optional) ");
      Put_Line ("return SelectionRange;");
      Put ("procedure Set (Self : in out SelectionRange_Optional; ");
      Put_Line ("Value : SelectionRange);");
      Put_Line ("procedure Clear (Self : in out SelectionRange_Optional);");
      New_Line;

      Put_Line ("private");
      New_Line;
      Put ("type DocumentSymbol_Array is array (Positive range <>) of ");
      Put_Line ("DocumentSymbol;");
      Put ("type DocumentSymbol_Array_Access is access all ");
      Put_Line ("DocumentSymbol_Array;");
      New_Line;

      Put ("type DocumentSymbol_Vector is ");
      Put_Line ("new Ada.Finalization.Controlled with record");
      Put_Line ("Data : DocumentSymbol_Array_Access;");
      Put_Line ("end record;");
      New_Line;

      Put ("overriding procedure Finalize (Self : in out ");
      Put_Line ("DocumentSymbol_Vector);");
      Put ("overriding procedure Adjust (Self : in out ");
      Put_Line ("DocumentSymbol_Vector);");
      New_Line;

      Put_Line ("type SelectionRange_Access is access all SelectionRange;");

      Put ("type SelectionRange_Optional is ");
      Put_Line ("new Ada.Finalization.Controlled with record");
      Put_Line ("   Value : SelectionRange_Access;");
      Put_Line ("end record;");

      Put ("overriding procedure Finalize (Self : in out ");
      Put_Line ("SelectionRange_Optional);");
      Put ("overriding procedure Adjust (Self : in out ");
      Put_Line ("SelectionRange_Optional);");

   end Write_Private_Part;

   ----------------
   -- Write_Enum --
   ----------------

   procedure Write_Enum
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set) is
   begin
      if Done.Contains (Name) then
         return;
      end if;

      Done.Insert (Name);

      if Enums (Name).Has_Optional then
         Write_Optional_Type (Name);
      end if;
   end Write_Enum;

   -----------------------
   -- Write_Enumeration --
   -----------------------

   procedure Write_Enumeration
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector) is
   begin
      Put ("type ");
      Put (Name);
      Put (" is (");

      for J in 1 .. List.Length loop
         if J > 1 then
            Put (", ");
         end if;

         Put (List (J).Union.stringLiteral.value);
      end loop;

      Put_Line (");");
   end Write_Enumeration;

   -----------------
   -- Write_Mixin --
   -----------------

   procedure Write_Mixin
     (Item : LSP_Gen.Entities.Structure;
      Done : in out String_Sets.Set) is
   begin
      Emit_Dependence (Item.properties, "", Done, Enclosing => Item.name);

      Put ("type ");
      Put_Id (Item.name);
      Put_Line (" is interface;");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;

      for J in 1 .. Item.properties.Length loop
         declare
            Property : constant LSP_Gen.Entities.Property :=
              Item.properties (J);
         begin
            Put ("function ");
            Put_Id (Property.name);
            Put (" (Self : ");
            Put_Id (Item.name);
            Put (") return ");
            Write_Type_Name (Property.a_type, Property.optional);
            Put_Line (" is abstract;");
            Put_Lines (Property.documentation.Split_Lines, "   --  ");
            New_Line;
         end;
      end loop;
   end Write_Mixin;

   ------------------
   -- Write_Mixins --
   ------------------

   procedure Write_Mixins (Done : in out String_Sets.Set) is
   begin
      for Item of Types loop
         if Item.Is_Mixin then
            Write_Mixin (Item.Definition, Done);
         end if;
      end loop;
   end Write_Mixins;

   -------------------------
   -- Write_Optional_Type --
   -------------------------

   procedure Write_Optional_Type (Name : VSS.Strings.Virtual_String) is
   begin
      if Name = "LSPAny" then
         Put_Line ("subtype LSPAny_Optional is LSPAny;");
         New_Line;
      elsif Name = "SelectionRange" then
         null;
      else
         Put ("type ");
         Put (Name);
         Put_Line ("_Optional (Is_Set : Boolean := False) is record");
         Put_Line ("case Is_Set is");
         Put_Line ("when False =>");
         Put_Line ("null;");
         Put_Line ("when True =>");
         Put ("Value : ");
         Put_Id (Name);
         Put_Line (";");
         Put_Line ("end case;");
         Put_Line ("end record;");
         New_Line;
      end if;
   end Write_Optional_Type;

   ------------------------
   -- Write_Or_Null_Type --
   ------------------------

   procedure Write_Or_Null_Type
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector)
   is
      Last     : constant LSP_Gen.Entities.AType := List (List.Length);
      Has_Null : constant Boolean := Last.Union.Kind = base
        and then Last.Union.base.name = LSP_Gen.Entities.Enum.a_null;
   begin
      if List.Length = 2 and then
        Has_Null and then
        List (1).Union.Kind in
          LSP_Gen.Entities.Enum.base | LSP_Gen.Entities.Enum.reference
      then
         Put ("type ");
         Put (Name);
         Put_Line (" (Is_Null : Boolean := True) is record");
         Put_Line ("case Is_Null is");
         Put_Line ("when True =>");
         Put_Line ("null;");
         Put_Line ("when False =>");
         Put ("Value : ");
         if List (1).Union.Kind in LSP_Gen.Entities.Enum.base then
            Put (Base_Full_Name (List (1).Union.base.name));
         else
            Put_Id (List (1).Union.reference.name);
         end if;
         Put_Line (";");
         Put_Line ("end case;");
         Put_Line ("end record;");
      end if;
   end Write_Or_Null_Type;

   ----------------------
   -- Write_Properties --
   ----------------------

   procedure Write_Properties
     (List        : LSP_Gen.Entities.Property_Vector;
      Is_Optional : Boolean := False;
      Enclosing   : VSS.Strings.Virtual_String) is
   begin
      for J in 1 .. List.Length loop
         Write_Property (List (J), Is_Optional, Enclosing);
      end loop;
   end Write_Properties;

   --------------------
   -- Write_Property --
   --------------------

   procedure Write_Property
     (Item        : LSP_Gen.Entities.Property;
      Is_Optional : Boolean := False;
      Enclosing   : VSS.Strings.Virtual_String) is
   begin
      if Item.name = "kind" and then
        Item.a_type.Union.Kind = stringLiteral
      then
         --  Don't generate tag for extended types
         return;
      end if;

      Put_Id (Item.name);
      Put (" : ");

      if Item.a_type.Union.Kind in literal | a_or | an_array | map then
         Put ("LSP.Structures.");
         Put (Short_Name
                (Item.a_type,
                 Fallback => Item.name & "_Of" & Enclosing));

         if Item.a_type.Union.Kind not in an_array | map and
           (Item.optional or Is_Optional)
         then
            Put ("_Optional");
         end if;
      else
         Write_Type_Name (Item.a_type, Item.optional or Is_Optional);
      end if;
      Put_Line (";");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;

      if Item.a_type.Union.Kind in an_array
        and then Item.a_type.Union.an_array.element.Value.Union.Kind
          = reference
        and then Enums.Contains
          (Item.a_type.Union.an_array.element.Value.Union.reference.name)
      then
         Put_Line (" --  enum array");
      end if;
   end Write_Property;

   ---------------------
   -- Write_Structure --
   ---------------------

   procedure Write_Structure
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set)
   is

      procedure Write_Mixins
        (Item   : LSP_Gen.Entities.Structure;
         Prefix : VSS.Strings.Virtual_String);

      procedure Write_Mixin_Propeties (Item : LSP_Gen.Entities.Structure);
      procedure Write_Mixin_Functions (Item : LSP_Gen.Entities.Structure);

      ---------------------------
      -- Write_Mixin_Functions --
      ---------------------------

      procedure Write_Mixin_Functions (Item : LSP_Gen.Entities.Structure) is
      begin
         for K in 1 .. Item.mixins.Length loop
            declare
               Mixin  : constant LSP_Gen.Entities.AType := Item.mixins (K);
               Parent : constant LSP_Gen.Entities.Structure :=
                 Types (Mixin.Union.reference.name).Definition;
            begin
               for J in 1 .. Parent.properties.Length loop
                  declare
                     Property : constant LSP_Gen.Entities.Property :=
                       Parent.properties (J);
                  begin
                     Put ("overriding function ");
                     Put_Id (Property.name);
                     Put (" (Self : ");
                     Put_Id (Item.name);
                     Put (") return ");
                     Write_Type_Name (Property.a_type, Property.optional);
                     Put (" is (Self.");
                     Put_Id (Property.name);
                     Put_Line (");");
                     New_Line;
                  end;
               end loop;
            end;
         end loop;
      end Write_Mixin_Functions;

      ---------------------------
      -- Write_Mixin_Propeties --
      ---------------------------

      procedure Write_Mixin_Propeties (Item : LSP_Gen.Entities.Structure) is
      begin
         for J in 1 .. Item.mixins.Length loop
            declare
               Mixin  : constant LSP_Gen.Entities.AType := Item.mixins (J);
               Parent : constant LSP_Gen.Entities.Structure :=
                 Types (Mixin.Union.reference.name).Definition;
            begin
               Write_Properties (Parent.properties, False, Name);
            end;
         end loop;
      end Write_Mixin_Propeties;

      ------------------
      -- Write_Mixins --
      ------------------

      procedure Write_Mixins
        (Item   : LSP_Gen.Entities.Structure;
         Prefix : VSS.Strings.Virtual_String) is
      begin
         for J in 1 .. Item.mixins.Length loop
            if J = 1 then
               Put (Prefix);
            else
               Put (" and ");
            end if;

            Put_Id (Item.mixins (J).Union.reference.name);
            New_Line;
         end loop;
      end Write_Mixins;

      Item : constant LSP_Gen.Entities.Structure := Types (Name).Definition;
   begin
      if Done.Contains (Name) or else Types (Name).Is_Mixin then
         return;
      end if;

      Emit_Dependence (Item.extends, Item.name, Done);
      Emit_Dependence (Item.mixins, Item.name, Done);
      Emit_Dependence (Item.properties, Item.name, Done, Enclosing => Name);

      Done.Insert (Name);
      Put ("type ");
      Put_Id (Name);
      Put_Line (" is ");

      if Name = "LSPObject" then
         Put ("new LSPAny with ");
      elsif Item.extends.Length > 0 then
         Put ("new ");
         Put_Id
           (Item.extends (Base_Index (Item.extends)).Union.reference.name);

         Write_Mixins (Item, " and ");
         Put (" with ");
      elsif Item.mixins.Length > 0 then
         Write_Mixins (Item, "new ");
         Put (" with ");
      elsif Types (Name).Is_Tagged then
         Put ("tagged ");
      end if;

      Put_Line ("record");

      Write_Mixin_Propeties (Item);

      if Item.extends.Length > 1 then
         pragma Assert (Item.extends.Length = 2);

         for J in 1 .. 2 loop
            if J /= Base_Index (Item.extends) then
               Put ("Parent : ");
               Write_Type_Name (Item.extends (J), False);
               Put_Line (";");
            end if;
         end loop;

         if Item.properties.Length > 0 then
            Write_Properties (Item.properties, Enclosing => Name);
         end if;
      else
         Write_Properties (Item.properties, Enclosing => Name);

         if Item.properties.Length = 0 and Item.mixins.Length = 0 then
            Put_Line ("null;");
         end if;
      end if;

      Put_Line ("end record;");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;
      Write_Mixin_Functions (Item);

      if Types (Name).Has_Optional then
         Write_Optional_Type (Name);
      end if;
   end Write_Structure;

   ------------------------
   -- Write_Two_Literals --
   ------------------------

   procedure Write_Two_Literals
     (Name   : VSS.Strings.Virtual_String;
      Base   : LSP_Gen.Entities.AType;
      Extend : LSP_Gen.Entities.AType)
   is
      Base_List : constant LSP_Gen.Entities.Property_Vector :=
        Base.Union.literal.value.properties;
      Extend_List : constant LSP_Gen.Entities.Property_Vector :=
        Extend.Union.literal.value.properties;
   begin
      --  This kind of `or` type used just once. In th usage one of literal
      --  extends another one literal. Let's generate it as a single record
      --  type with all added properties are optional.
      Put ("type ");
      Put (Name);
      Put_Line (" is record");

      for J in 1 .. Extend_List.Length loop
         Write_Property
           (Extend_List (J),
            Is_Optional => not
              (for some K in 1 .. Base_List.Length =>
                 Base_List (K).name = Extend_List (J).name),
            Enclosing   => Name);
      end loop;

      Put_Line ("end record;");
   end Write_Two_Literals;

   ---------------------
   -- Write_Two_Types --
   ---------------------

   procedure Write_Two_Types
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector;
      Fallback_2 : VSS.Strings.Virtual_String := "")
   is
      Last : constant LSP_Gen.Entities.AType := List (List.Length);
      First_Name : constant VSS.Strings.Virtual_String :=
        Short_Name (List (1));
      Second_Name : constant VSS.Strings.Virtual_String :=
        Short_Name (Last, Fallback_2);
   begin
      Put ("type ");
      Put (Name);
      Put (" (Is_");
      Put (First_Name);
      Put_Line (" : Boolean := True) is record");
      Put ("case Is_");
      Put (First_Name);
      Put_Line (" is");
      Put_Line ("   when True =>");
      Put (First_Name);
      Put (": ");
      Write_Type_Name (List (1), False);
      Put_Line (";");
      Put_Line ("   when False =>");
      Put (Second_Name);
      Put (": ");

      if Fallback_2.Is_Empty then
         Write_Type_Name (Last, False);
      else
         Put ("LSP.Structures.");
         Put (Second_Name);
      end if;

      Put_Line (";");
      Put_Line ("end case;");
      Put_Line ("end record;");
      New_Line;
   end Write_Two_Types;

   ----------------
   -- Write_Type --
   ----------------

   procedure Write_Type
     (Name     : VSS.Strings.Virtual_String;
      Item     : LSP_Gen.Entities.AType;
      Fallback : VSS.Strings.Virtual_String) is
   begin
      case Item.Union.Kind is
         when base =>
            Put ("type ");
            Put_Id (Name);
            Put (" is new ");
            case Item.Union.base.name is
               when LSP_Gen.Entities.Enum.string =>
                  Put_Line ("VSS.Strings.Virtual_String with null record;");
               when others =>
                  raise Program_Error;
            end case;
         when an_array =>
            declare
               Element : constant VSS.Strings.Virtual_String := Short_Name
                   (Item.Union.an_array.element.Value, Fallback & "_Item");
            begin
               if Name /= "LSPAny_Vector"
                 and Name /= "Virtual_String_Vector"
               then
                  Write_Vector_Type (Name, Element);
               end if;
            end;
         when map =>
            declare
               Element : constant VSS.Strings.Virtual_String := Short_Name
                   (Item.Union.map.value.Value, Fallback & "_Item");
            begin
               Put ("package ");
               Put (Element);
               Put ("_Maps is new Ada.Containers.Hashed_Maps (");
               --
               Put (Short_Name (Item.Union.map.key));
               Put (", ");
               Put (Element);
               Put (", Get_Hash, ""=""");
               Put_Line (");"); New_Line;

               Put ("type ");
               Put_Id (Name);
               Put (" is new ");
               Put (Element);
               Put_Line ("_Maps.Map with null record;");
            end;
         when a_or =>
            declare
               Map : constant Or_Mapping :=
                 Get_Or_Mapping (Item.Union.a_or.items);
            begin
               if Name = "LSPAny" or Name = "LSPArray" then
                  Put ("type ");
                  Put_Id (Name);
                  Put_Line
                    (" is new JSON_Event_Vectors.Vector with null record;");
               else
                  case Map.Kind is
                     when Type_Class =>
                        Write_Class_Type (Name, Map.Tipe);

                     when Type_Union =>
                        Write_Union (Name, Map.Items);

                     when Type_Or_Null =>
                        Write_Or_Null_Type (Name, Item.Union.a_or.items);

                     when Two_Types =>
                        Write_Two_Types (Name, Item.Union.a_or.items);
                     when Option_Combination =>
                        Put ("type ");
                        Put_Id (Name);
                        Put_Line (" is record");
                        Write_Properties
                          (Map.Tipe.Union.literal.value.properties,
                           True,
                           Name);
                        Put_Line ("end record;");
                     when String_Or_Something =>
                        Write_Two_Types
                          (Name, Item.Union.a_or.items, Fallback & "_Literal");

                     when String_Or_Tuple =>
                        Write_Two_Types (Name, Item.Union.a_or.items, "Tuple");

                     when Two_Literals =>
                        Write_Two_Literals (Name, Map.First, Map.Second);

                     when Boolean_Or_Something =>
                        Write_Two_Types
                          (Name, Item.Union.a_or.items, Fallback & "_Literal");

                     when Boolean_Or_Class =>
                        Write_Boolean_Or_Class (Name, Map.Tipe);

                     when Enumeration =>
                        Write_Enumeration (Name, Map.Items);

                     when Array_Or_Null =>

                        Put ("subtype ");
                        Put_Id (Name);
                        Put (" is ");
                        Put (Short_Name (Map.Array_Type));
                        Put_Line (";");
                        Put_Line ("--  Send an empty array instead of `null`");
                     when others =>
                        null;
                  end case;
               end if;
            end;
         when tuple =>
            Put ("type ");
            Put (Name);
            Put (" is array (1 .. ");
            Put (Item.Union.tuple.items.Length);
            Put (") of ");
            Put (Short_Name (Item.Union.tuple.items (1)));
            Put_Line (";");

         when literal =>
            Put ("type ");
            Put_Id (Name);
            Put_Line (" is record");
            Write_Properties
              (Item.Union.literal.value.properties, Enclosing => Name);
            Put ("end record;");
         when others =>
            raise Program_Error;
      end case;
   end Write_Type;

   ---------------------
   -- Write_Type_Name --
   ---------------------

   procedure Write_Type_Name
     (Item        : LSP_Gen.Entities.AType;
      Is_Optional : Boolean) is
   begin
      case Item.Union.Kind is
         when base =>
            if Is_Optional then
               Put (Base_Short_Name (Item.Union.base.name));
               Put ("_Optional");
            else
               Put (Base_Full_Name (Item.Union.base.name));
            end if;
         when reference =>
            if not Enums.Contains (Item.Union.reference.name)
            then
               Put ("LSP.Structures.");
            elsif not Is_Optional then
               Put ("LSP.Enumerations.");
            end if;

            if Is_Optional then
               Put (Item.Union.reference.name);
               Put ("_Optional");
            else
               Put_Id (Item.Union.reference.name);
            end if;
         when an_array =>
            --  Could be Is_Optional!!!
            Put ("LSP.Structures.");
            Put (Short_Name (Item));
         when map =>
            --  Could be Is_Optional!!!
            Write_Type_Name
              (Item.Union.map.value.Value, False);
            Put ("_Maps.Map");
         when a_or | tuple =>
            Put (Short_Name (Item));
         when others =>
            raise Program_Error;
      end case;
   end Write_Type_Name;

   -----------------
   -- Write_Types --
   -----------------

   procedure Write_Types (Model : LSP_Gen.Entities.MetaModel) is
      List    : LSP_Gen.Entities.Structure_Vector renames Model.structures;
      Aliases : LSP_Gen.Entities.TypeAlias_Vector renames Model.typeAliases;

      Done : String_Sets.Set;
   begin
      --  Put structures into Types
      for J in 1 .. List.Length loop
         declare
            Item : constant LSP_Gen.Entities.Structure := List (J);
         begin
            Types.Insert (Item.name, (Item.name, Item, others => <>));
         end;
      end loop;

      for J in 1 .. Aliases.Length loop
         declare
            Item : constant LSP_Gen.Entities.TypeAlias := Aliases (J);
         begin
            LSP_Gen.Type_Aliases.Aliases.Insert
              (Item.name, (Item.name, Item, others => <>));
         end;
      end loop;

      Find_Optional_And_Arrays;
      Done.Insert ("LSPAny_Optional");

      Put_Line ("with Ada.Containers.Hashed_Maps;");
      Put_Line ("with Ada.Containers.Vectors;");
      Put_Line ("with Ada.Finalization;"); New_Line;
      Put_Line ("with VSS.JSON.Events;");
      Put_Line ("with VSS.Strings;"); New_Line;
      Put_Line ("with VSS.String_Vectors;"); New_Line;
      Put_Line ("with LSP.Enumerations; use LSP.Enumerations;"); New_Line;

      Put_Line ("package LSP.Structures is"); New_Line;

      Put_Line ("subtype Virtual_String is VSS.Strings.Virtual_String;");
      Put_Line ("subtype Virtual_String_Optional is Virtual_String;");
      Put ("subtype Virtual_String_Vector is");
      Put_Line (" VSS.String_Vectors.Virtual_String_Vector;");
      New_Line;

      Put ("type DocumentUri");
      Put_Line (" is new VSS.Strings.Virtual_String with null record;");
      New_Line;
      Put ("function Get_Hash (Self : DocumentUri) return");
      Put_Line (" Ada.Containers.Hash_Type is");
      Put_Line (" (Ada.Containers.Hash_Type'Mod (Self.Hash));");
      New_Line;

      Put_Line ("package JSON_Event_Vectors is new Ada.Containers.Vectors");
      Put_Line
        ("(Positive, VSS.JSON.Events.JSON_Event, VSS.JSON.Events.""="");");
      New_Line;

      Put_Line ("type SelectionRange_Optional is tagged private;");
      New_Line;

      Write_Optional_Type ("Boolean");
      Write_Optional_Type ("Natural");
      Write_Optional_Type ("Integer");

      Write_Mixins (Done);

      for Cursor in Types.Iterate loop
         Write_Structure (Type_Maps.Key (Cursor), Done);
      end loop;

      Write_Private_Part;
      Put_Line ("end LSP.Structures;");
   end Write_Types;

   ----------------------
   -- Write_Type_Alias --
   ----------------------

   procedure Write_Type_Alias
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set)
   is
      Item : constant LSP_Gen.Entities.TypeAlias := Aliases (Name).Definition;
   begin
      if Done.Contains (Name) then
         return;
      end if;

      Emit_Dependence (Item.a_type, "", Done, Name, With_Type => True);

      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;

      if Item.a_type.Union.Kind = base and then
        Item.a_type.Union.base.name = LSP_Gen.Entities.Enum.string
      then
         Put ("function Get_Hash (Self : ");
         Put (Name);
         Put (") return");
         Put_Line (" Ada.Containers.Hash_Type is");
         Put_Line (" (Ada.Containers.Hash_Type'Mod (Self.Hash));");
         New_Line;
      end if;

      if Aliases (Name).Has_Optional then
         if Item.a_type.Union.Kind = base
           and then Item.a_type.Union.base.name = LSP_Gen.Entities.Enum.string
         then
            Put ("subtype ");
            Put_Id (Name);
            Put ("_Optional is ");
            Put_Id (Name);
            Put_Line (";");
            New_Line;
         else
            Write_Optional_Type (Name);
         end if;
      end if;

      if Name = "LSPAny" then
         Put_Line ("subtype LSPAny_Vector is LSPAny;");
         New_Line;
      end if;
   end Write_Type_Alias;

   -----------------
   -- Write_Union --
   -----------------

   procedure Write_Union
     (Name : VSS.Strings.Virtual_String;
      List : LSP_Gen.Entities.AType_Vector)
   is
      function Get_Variant
        (Item : LSP_Gen.Entities.AType) return VSS.Strings.Virtual_String;
      --  Return variant name for given `or` type item

      -----------------
      -- Get_Variant --
      -----------------

      function Get_Variant
        (Item : LSP_Gen.Entities.AType) return VSS.Strings.Virtual_String
      is
         First : constant LSP_Gen.Entities.Property :=
           Types (Item.Union.reference.name).Definition.properties (1);
      begin
         if First.name = "kind" then
            return First.a_type.Union.stringLiteral.value;
         else
            return "Default";
         end if;
      end Get_Variant;

      Variants : VSS.String_Vectors.Virtual_String_Vector;
   begin
      for J in 1 .. List.Length loop
         Variants.Append (Get_Variant (List (J)));
      end loop;

      Put ("type ");
      Put (Name);
      Put ("_Variant is (");

      for J in 1 .. Variants.Length loop
         if J > 1 then
            Put (", ");
         end if;

         Put_Id (Variants (J));
      end loop;
      Put_Line (");");
      New_Line;

      Put ("type ");
      Put (Name);
      Put (" (Kind : ");
      Put (Name);
      Put ("_Variant := ");
      Put (Name);
      Put_Line ("_Variant'First) is record");
      Put_Line ("case Kind is");

      for J in 1 .. Variants.Length loop
         Put ("when ");
         Put_Id (Variants (J));
         Put_Line (" =>");
         Put_Id (Variants (J));
         Put (" : ");
         Write_Type_Name (List (J), False);
         Put_Line (";");
      end loop;

      Put_Line ("end case;");
      Put_Line ("end record;");
      New_Line;
   end Write_Union;

   -----------------------
   -- Write_Vector_Type --
   -----------------------

   procedure Write_Vector_Type
     (Name : VSS.Strings.Virtual_String;
      Item : VSS.Strings.Virtual_String) is
   begin
      if Name = "DocumentSymbol_Vector" then
         Put ("type ");
         Put (Name);
         Put_Line (" is tagged private with");
         Put_Line
           ("Variable_Indexing => Get_DocumentSymbol_Variable_Reference,");
         Put_Line
           ("Constant_Indexing => Get_DocumentSymbol_Constant_Reference;");
         New_Line;
         return;
      elsif Enums.Contains (Item) then
         --  It looks like any enum array in LSP is a set. Let's define them
         --  as sets.

         Put ("type ");
         Put (Name);
         Put (" is array (");
         Put (Item);
         Put_Line (") of Boolean");
         Put_Line ("  with Pack, Default_Component_Value => False;");
         New_Line;
         return;
      end if;

      Put ("package ");
      Put (Item);
      Put ("_Vectors is new Ada.Containers.Vectors (Positive, ");
      Put_Id (Item);
      Put_Line (", ""="");");
      New_Line;

      Put ("type ");
      Put (Name);
      Put_Line (" is new ");
      Put (Item);
      Put_Line ("_Vectors.Vector with null record;");
      New_Line;
   end Write_Vector_Type;

end LSP_Gen.Structures;
