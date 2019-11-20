open Belt;

module type PdfAttachment = {
  type t = {
    name: string,
    bytes: array(string) // string = byte
  };
};

module ListUtils = {
  let sumInt = list => List.reduce(list, 0, (a, b) => a + b);
  let sumFloat = list => List.reduce(list, 0.0, (a, b) => a +. b);
};

module ConstrainedType = {
  let createString = (fieldName, ctor, maxLen, str) =>
    switch (str) {
    | "" => Result.Error({j|"$fieldName must not be empty"|j})
    | str when str->Js.String2.length > maxLen =>
      Result.Error({j|"$fieldName must not be  more than $maxLen chars"|j})
    | _ => Result.Ok(ctor)
    };

  let createStringOption = (fieldName, ctor, maxLen, str) =>
    switch (str) {
    | "" => Result.Ok(None)
    | str when str->Js.String2.length > maxLen =>
      Result.Error({j|"$fieldName must not be  more than $maxLen chars"|j})
    | _ => Result.Ok(ctor)
    };

  let createInt = (fieldName, ctor, minVal, maxVal, i: int) =>
    if (i < minVal) {
      Result.Error({j|"$fieldName must not be less than $minVal"|j});
    } else if (i > maxVal) {
      Result.Error({j|"$fieldName must not be greater than $maxVal"|j});
    } else {
      Result.Ok(ctor);
    };

  let createFloat = (fieldName, ctor, minVal, maxVal, i: float) =>
    if (i < minVal) {
      Result.Error({j|"$fieldName must not be less than $minVal"|j});
    } else if (i > maxVal) {
      Result.Error({j|"$fieldName must not be greater than $maxVal"|j});
    } else {
      Result.Ok(ctor);
    };

  let createLike = (fieldName, ctor, pattern, str) =>
    switch (str) {
    | "" => Result.Error({j|"$fieldName must not be empty"|j})
    | str when str->Js.String2.match(pattern->Js.Re.fromString)->Option.isSome =>
      Result.Ok(ctor)
    | _ =>
      Result.Error(
        {j|"$fieldName: ' $str ' must match the pattern ' $pattern '"|j},
      )
    };
};

module type String50 = {
  type t = pri | String50(string);
  let value: t => string;
  let create: (string, string) => Belt.Result.t(t, string);
  let createOption: (string, string) => Belt.Result.t(t, string);
};

module String50 = {
  type t =
    | String50(string);

  let value = (String50(str)) => str;

  let create = (fieldName, str) =>
    ConstrainedType.createString(fieldName, String50(str), 50, str);

  let createOption = (fieldName, str) =>
    ConstrainedType.createStringOption(
      fieldName,
      Some(String50(str)),
      50,
      str,
    );
};

module type EmailAddress = {
  type t = pri | EmailAddress(string);
  let value: t => string;
  let create: (string, string) => Belt.Result.t(t, string);
};

module EmailAddress: EmailAddress = {
  type t =
    | EmailAddress(string);

  let value = (EmailAddress(str)) => str;

  let create = (fieldName, str) => {
    let pattern = ".+@.+";
    ConstrainedType.createLike(fieldName, EmailAddress(str), pattern, str);
  };
};

module type ZipCode = {
  type t = pri | ZipCode(string);
  let value: t => string;
  let create: (string, string) => Belt.Result.t(t, string);
};

module ZipCode = {
  type t =
    | ZipCode(string);

  let value = (ZipCode(str)) => str;

  let create = (fieldName, str) => {
    let pattern = "\d{5}";
    ConstrainedType.createLike(fieldName, ZipCode(str), pattern, str);
  };
};

module type OrderId = {
  type t = pri | OrderId(string);
  let value: t => string;
  let create: (string, string) => Belt.Result.t(t, string);
};

module OrderId: OrderId = {
  type t =
    | OrderId(string);

  let value = (OrderId(str)) => str;

  let create = (fieldName, str) =>
    ConstrainedType.createString(fieldName, OrderId(str), 50, str);
};

module type OrderLineId = {
  type t = pri | OrderLineId(string);
  let value: t => string;
  let create: (string, string) => Belt.Result.t(t, string);
};

module OrderLineId: OrderLineId = {
  type t =
    | OrderLineId(string);

  let value = (OrderLineId(str)) => str;

  let create = (fieldName, str) =>
    ConstrainedType.createString(fieldName, OrderLineId(str), 50, str);
};

module type WidgetCode = {
  type t = pri | WidgetCode(string);
  let value: t => string;
  let create: (string, string) => Belt.Result.t(t, string);
};

module WidgetCode: WidgetCode = {
  type t =
    | WidgetCode(string);

  let value = (WidgetCode(str)) => str;

  let create = (fieldName, code) => {
    let pattern = "W\d{4}";
    ConstrainedType.createLike(fieldName, WidgetCode(code), pattern, code);
  };
};

module type GizmoCode = {
  type t = pri | GizmoCode(string);
  let value: t => string;
  let create: (string, string) => Belt.Result.t(t, string);
};

module GizmoCode: GizmoCode = {
  type t =
    | GizmoCode(string);

  let value = (GizmoCode(str)) => str;

  let create = (fieldName, code) => {
    let pattern = "G\d{3}";
    ConstrainedType.createLike(fieldName, GizmoCode(code), pattern, code);
  };
};

module type ProductCode = {
  type t = pri | Widget(WidgetCode.t) | Gizmo(GizmoCode.t);
  let value: t => string;
  let create: (string, string) => Belt.Result.t(t, string);
};

module ProductCode: ProductCode = {
  type t =
    | Widget(WidgetCode.t)
    | Gizmo(GizmoCode.t);

  let value =
    fun
    | Widget(WidgetCode(wc)) => wc
    | Gizmo(GizmoCode(gc)) => gc;

  let create = (fieldName, code) => {
    switch (code) {
    | "" => Result.Error({j|$fieldName: Must not be null or empty|j})
    | code when code->Js.String2.startsWith("W") =>
      WidgetCode.create(fieldName, code)->Result.map(code => Widget(code))
    | code when code->Js.String2.startsWith("G") =>
      GizmoCode.create(fieldName, code)->Result.map(code => Gizmo(code))
    | _ => Result.Error({j|$fieldName: Format not recognized ' $code '|j})
    };
  };
};

module type UnitQuantity = {
  type t = pri | UnitQuantity(int);
  let value: t => int;
  let create: (string, int) => Belt.Result.t(t, string);
};

module UnitQuantity: UnitQuantity = {
  type t =
    | UnitQuantity(int);

  let value = (UnitQuantity(v)) => v;

  let create = (fieldName, v) => {
    ConstrainedType.createInt(fieldName, UnitQuantity(v), 1, 1000, v);
  };
};

module type KilogramQuantity = {
  type t = pri | KilogramQuantity(float);
  let value: t => float;
  let create: (string, float) => Belt.Result.t(t, string);
};

module KilogramQuantity: KilogramQuantity = {
  type t =
    | KilogramQuantity(float);

  let value = (KilogramQuantity(v)) => v;

  let create = (fieldName, v) => {
    ConstrainedType.createFloat(
      fieldName,
      KilogramQuantity(v),
      0.5,
      100.0,
      v,
    );
  };
};

module type OrderQuantity = {
  type t = pri | Unit(UnitQuantity.t) | Kilogram(KilogramQuantity.t);
  let value: t => float;
  let create: (string, ProductCode.t, float) => Belt.Result.t(t, string);
};

module OrderQuantity: OrderQuantity = {
  type t =
    | Unit(UnitQuantity.t)
    | Kilogram(KilogramQuantity.t);

  let value =
    fun
    | Unit(UnitQuantity(uq)) => uq->float_of_int
    | Kilogram(KilogramQuantity(kq)) => kq;

  let create = (fieldName, productCode, quantity) =>
    switch (productCode) {
    | ProductCode.Widget(_) =>
      UnitQuantity.create(fieldName, quantity->int_of_float)
      ->Result.map(qty => Unit(qty))
    | ProductCode.Gizmo(_) =>
      KilogramQuantity.create(fieldName, quantity)
      ->Result.map(qty => Kilogram(qty))
    };
};

module type Price = {
  type t = pri | Price(float);
  let value: t => float;
  let create: float => Belt.Result.t(t, string);
  let createExn: float => t;
};

module Price: Price = {
  type t =
    | Price(float);

  let value = (Price(v)) => v;

  let create = v =>
    ConstrainedType.createFloat("Price", Price(v), 0.0, 1000.0, v);

  let createExn = v => create(v)->Result.getExn;
};

module type BillingAmount = {
  type t = pri | BillingAmount(float);
  let value: t => float;
  let create: float => Belt.Result.t(t, string);
  let sumPrices: list(Price.t) => Belt.Result.t(t, string);
};

module BillingAmount: BillingAmount = {
  type t =
    | BillingAmount(float);

  let value = (BillingAmount(v)) => v;

  /// Create a BillingAmount from a decimal.
  /// Return Error if input is not a decimal between 0.0 and 10000.00
  let create = v =>
    ConstrainedType.createFloat(
      "BillingAmount",
      BillingAmount(v),
      0.0,
      10000.0,
      v,
    );

  /// Sum a list of prices to make a billing amount
  /// Return Error if total is out of bounds
  let sumPrices = prices =>
    prices->List.map(price => Price.value(price))->ListUtils.sumFloat->create;
};