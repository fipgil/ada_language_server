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
   procedure Write_Mixins;
   procedure Write_Mixin (Item : LSP_Gen.Entities.Structure);
   procedure Write_Structure
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);
   procedure Write_Type_Alias
     (Name : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);
   procedure Write_Properties
     (List        : LSP_Gen.Entities.Property_Vector;
      Is_Optional : Boolean := False);
   --  Force all properties to be optional if Is_Optional
   procedure Write_Property
     (Item        : LSP_Gen.Entities.Property;
      Is_Optional : Boolean := False);
   --  Force property to be optional if Is_Optional
   procedure Write_Type_Name
     (Item        : LSP_Gen.Entities.AType;
      Is_Optional : Boolean);
   procedure Write_Or_Type
     (List : LSP_Gen.Entities.AType_Vector;
      Done : in out String_Sets.Set);
   procedure Emit_Dependence
     (Item : LSP_Gen.Entities.AType;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);

   procedure Emit_Dependence
     (Item : LSP_Gen.Entities.AType_Vector;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);

   procedure Emit_Dependence
     (Item : LSP_Gen.Entities.Property_Vector;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set);

   function Get_Or_Mapping
     (Items : LSP_Gen.Entities.AType_Vector) return Or_Mapping;

   function Is_Extents
     (Child, Parent : LSP_Gen.Entities.AType) return Boolean;
   --  Check if Child structure extends Parent

   function Has_Kind (Item : LSP_Gen.Entities.AType) return Boolean;
   --  the type has `kind` stringLiteral property

   function Short_Name
     (Item : LSP_Gen.Entities.AType) return VSS.Strings.Virtual_String;
   --  Return simple defining name for `base` and `reference` types

   function Base_Index (List : LSP_Gen.Entities.AType_Vector) return Positive;
   --  Return index of base type from given `extends` list.

   Base_Short_Name : constant array (LSP_Gen.Entities.Enum.BaseTypes) of
     VSS.Strings.Virtual_String :=
       (LSP_Gen.Entities.Enum.Uri |
        LSP_Gen.Entities.Enum.DocumentUri => "IRI",
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
        LSP_Gen.Entities.Enum.DocumentUri => "VSS.IRIs.IRI",
        LSP_Gen.Entities.Enum.integer     => "Integer",
        LSP_Gen.Entities.Enum.uinteger    => "Natural",
        LSP_Gen.Entities.Enum.decimal     => "Float",
        LSP_Gen.Entities.Enum.RegExp |
        LSP_Gen.Entities.Enum.string      => "VSS.Strings.Virtual_String",
        LSP_Gen.Entities.Enum.a_boolean   => "Boolean",
        LSP_Gen.Entities.Enum.a_null      => "???");

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
     (Item : LSP_Gen.Entities.AType;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set) is
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
               end if;
            end if;
         when an_array =>
            Emit_Dependence (Item.Union.an_array.element.Value, Skip, Done);

         when a_or =>
            Emit_Dependence (Item.Union.a_or.items, Skip, Done);
            Write_Or_Type (Item.Union.a_or.items, Done);

         when literal =>
            Emit_Dependence (Item.Union.literal.value.properties, Skip, Done);

         when map =>
            Emit_Dependence (Item.Union.map.value.Value, Skip, Done);

         when tuple =>
            Emit_Dependence (Item.Union.tuple.items, Skip, Done);

         when an_and |
              integerLiteral | booleanLiteral =>
            raise Program_Error;
      end case;
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
     (Item : LSP_Gen.Entities.Property_Vector;
      Skip : VSS.Strings.Virtual_String;
      Done : in out String_Sets.Set) is
   begin
      for J in 1 .. Item.Length loop
         Emit_Dependence (Item (J).a_type, Skip, Done);
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
         return (String_Or_Array, Items (2));
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

      --  A union of boolean and some literal
      if Items.Length = 2 and then
        Items (1).Union.Kind = base and then
        Items (1).Union.base.name = LSP_Gen.Entities.Enum.a_boolean and then
        Items (2).Union.Kind = literal
      then
         return (Boolean_Or_Something, Items (2));
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

      --  All items are stringLiteral
      if (for all J in 1 .. Items.Length =>
            Items (J).Union.Kind = stringLiteral)
      then
         return (Enumeration, Items);
      end if;

      --  A union of `Location` and some literal
      if Items.Length = 2 and then
        Items (1).Union.Kind = reference and then
        Items (1).Union.reference.name = "Location" and then
        Items (2).Union.Kind = literal
      then
         return (Location_Or_Something, Items (2));
      end if;

      --  A union of two literals
      if Items.Length = 2 and then
        (for all J in 1 .. Items.Length => Items (J).Union.Kind = literal)
      then
         return (Kind => Two_Literals);
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
     (Item : LSP_Gen.Entities.AType) return VSS.Strings.Virtual_String is
   begin
      case Item.Union.Kind is
         when base =>
            return Base_Short_Name (Item.Union.base.name);
         when reference =>
            return Item.Union.reference.name;
         when an_array =>
            return Short_Name (Item.Union.an_array.element.Value) & "_Vector";
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
                  when String_Or_Array =>
                     return "String_Or_" & Short_Name (Mapping.Array_Type);
                  when String_Or_Tuple =>
                     return "String_Or_Tuple";
                  when Boolean_Or_Any =>
                     return "Boolean_Or_Any";
                  when Location_Or_Something =>
                     return "Location_Or_Something";
                  when Type_Union =>
                     return "Some_Union";
                  when Two_Literals
                     | String_Or_Something
                     | Boolean_Or_Something
                     | Boolean_Or_Class
                     | Enumeration
                     | Unknown_Mapping =>
                     raise Program_Error;
               end case;
            end;
         when others =>
            raise Program_Error;
      end case;
   end Short_Name;

   -----------------
   -- Write_Mixin --
   -----------------

   procedure Write_Mixin (Item : LSP_Gen.Entities.Structure) is
   begin
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
            Put ("--  function ");
            Put_Id (Property.name);
            Put (" (Self : ");
            Put_Id (Item.name);
            Put (") return ");
            Put_Type (Property.a_type);
            Put_Line (" is abstract;");
            Put_Lines (Property.documentation.Split_Lines, "   --  ");
            New_Line;
         end;
      end loop;
   end Write_Mixin;

   ------------------
   -- Write_Mixins --
   ------------------

   procedure Write_Mixins is
   begin
      for Item of Types loop
         if Item.Is_Mixin then
            Write_Mixin (Item.Definition);
         end if;
      end loop;
   end Write_Mixins;

   -------------------
   -- Write_Or_Type --
   -------------------

   procedure Write_Or_Type
     (List : LSP_Gen.Entities.AType_Vector;
      Done : in out String_Sets.Set)
   is
      Last     : constant LSP_Gen.Entities.AType := List (List.Length);
      Has_Null : constant Boolean := Last.Union.Kind = base
        and then Last.Union.base.name = LSP_Gen.Entities.Enum.a_null;
   begin
      if List.Length = 2 and then
        Has_Null and then
        List (1).Union.Kind = LSP_Gen.Entities.Enum.base
      then
         declare
            Item_Name : constant VSS.Strings.Virtual_String :=
              Base_Short_Name (List (1).Union.base.name);
            Type_Name : constant VSS.Strings.Virtual_String :=
              Item_Name & "_Or_Null";
         begin
            if not Done.Contains (Type_Name) then
               Done.Insert (Type_Name);

               Put ("type ");
               Put (Type_Name);
               Put_Line (" (Is_Null : Boolean := True) is record");
               Put_Line ("case Is_Null is");
               Put_Line ("when True =>");
               Put_Line ("null;");
               Put_Line ("when False =>");
               Put ("Value : ");
               Put (Base_Full_Name (List (1).Union.base.name));
               Put_Line (";");
               Put_Line ("end case;");
               Put_Line ("end record;");
               New_Line;
            end if;
         end;
      end if;
   end Write_Or_Type;

   ----------------------
   -- Write_Properties --
   ----------------------

   procedure Write_Properties
     (List        : LSP_Gen.Entities.Property_Vector;
      Is_Optional : Boolean := False) is
   begin
      if List.Length = 0 then
         Put_Line ("null;");
      end if;

      for J in 1 .. List.Length loop
         Write_Property (List (J), Is_Optional);
      end loop;
   end Write_Properties;

   --------------------
   -- Write_Property --
   --------------------

   procedure Write_Property
     (Item        : LSP_Gen.Entities.Property;
      Is_Optional : Boolean := False) is
   begin
      if Item.name = "kind" and then
        Item.a_type.Union.Kind = stringLiteral
      then
         --  Don't generate tag for extended types
         return;
      end if;

      Put_Id (Item.name);
      Put (" : ");

      if Item.a_type.Union.Kind = literal or else
        (Item.a_type.Union.Kind = a_or
         and then Get_Or_Mapping (Item.a_type.Union.a_or.items).Kind in
           Enumeration | Boolean_Or_Something | Boolean_Or_Class) or else
        (Item.a_type.Union.Kind = an_array
         and then Item.a_type.Union.an_array.element.Value.Union.Kind = a_or
         and then Get_Or_Mapping
          (Item.a_type.Union.an_array.element.Value.Union.a_or.items).Kind =
           Option_Combination)
      then
         Put ("LSP.Structures.");
         Put_Id (Item.name);

         if Item.optional or Is_Optional then
            Put ("_Optional");
         end if;
      else
         Write_Type_Name (Item.a_type, Item.optional or Is_Optional);
      end if;
      Put_Line (";");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;
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
      Emit_Dependence (Item.properties, Item.name, Done);

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
            Write_Properties (Item.properties);
         end if;
      else
         Write_Properties (Item.properties);
      end if;

      Put_Line ("end record;");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;
   end Write_Structure;

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
            Put ("LSP.Structures.");
            Put_Id (Item.Union.reference.name);

            if Is_Optional then
               Put ("_Optional");
            end if;
         when an_array =>
            --  Could be Is_Optional!!!
            Write_Type_Name
              (Item.Union.an_array.element.Value, False);
            Put ("_Vector");
         when map =>
            --  Could be Is_Optional!!!
            Write_Type_Name
              (Item.Union.map.value.Value, False);
            Put ("_Maps.Map");
         when a_or =>
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
      Put_Line ("with Ada.Containers.Vectors;"); New_Line;
      Put_Line ("with VSS.JSON.Events;"); New_Line;
      Put_Line ("with VSS.Strings;"); New_Line;
      Put_Line ("with VSS.IRIs;"); New_Line;

      Put_Line ("package LSP.Structures is"); New_Line;

      Write_Mixins;

      for Cursor in Types.Iterate loop
         Write_Structure (Type_Maps.Key (Cursor), Done);
      end loop;

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

      if Name /= "LSPAny" and Name /= "LSPArray" then
         Emit_Dependence (Item.a_type, "", Done);
      end if;

      Done.Insert (Name);

      case Item.a_type.Union.Kind is
         when base =>
            Put ("type ");
            Put_Id (Name);
            Put (" is new ");
            case Item.a_type.Union.base.name is
               when LSP_Gen.Entities.Enum.string =>
                  Put ("VSS.Strings.Virtual_String with null record");
               when others =>
                  raise Program_Error;
            end case;
         when an_array =>
            Put ("package ");
            Put (Short_Name (Item.a_type.Union.an_array.element.Value));
            Put ("_Vectors is new Ada.Containers.Vectors (Positive, ");
            Put (Short_Name (Item.a_type.Union.an_array.element.Value));
            Put_Line (");"); New_Line;
            Put ("type ");
            Put_Id (Name);
            Put (" is new ");
            Put (Short_Name (Item.a_type.Union.an_array.element.Value));
            Put ("_Vectors.Vector with null record");
         when a_or =>
            declare
               Map : constant Or_Mapping :=
                 Get_Or_Mapping (Item.a_type.Union.a_or.items);
            begin
               Put ("type ");
               Put_Id (Name);

               if Name = "LSPAny" or Name = "LSPArray" then
                  Put (" is new JSON_Event_Vectors.Vector with null record");
               else
                  case Map.Kind is
                     when Option_Combination =>
                        Put_Line (" is record");
                        Write_Properties
                          (Map.Tipe.Union.literal.value.properties, True);
                        Put ("end record");
                     when Two_Types =>
                        Put (" (Is_");
                        Put (Short_Name (Map.First));
                        Put_Line (" : Boolean := True) is record");
                        Put ("case Is_");
                        Put (Short_Name (Map.First));
                        Put_Line (" is");
                        Put_Line ("   when True =>");
                        Put (Short_Name (Map.First));
                        Put (" : ");
                        Write_Type_Name (Map.First, False);
                        Put_Line (";");
                        Put_Line ("   when False =>");
                        Put (Short_Name (Map.Second));
                        Put (" : ");
                        Write_Type_Name (Map.Second, False);
                        Put_Line (";");
                        Put_Line ("end case;");
                        Put ("end record");
                     when Two_Literals | String_Or_Something =>
                        Put (" is new I_Dont_Know");
                     when others =>
                        raise Program_Error;
                  end case;
               end if;
            end;
         when others =>
            raise Program_Error;
      end case;

      Put_Line (";");
      Put_Lines (Item.documentation.Split_Lines, "   --  ");
      New_Line;
   end Write_Type_Alias;

end LSP_Gen.Structures;
