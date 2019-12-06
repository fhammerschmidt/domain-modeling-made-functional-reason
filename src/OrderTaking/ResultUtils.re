open Belt.Result;

// Identity function.
let id = x => x;

module Result = {
  type t('success, 'failure) = Belt.Result.t('success, 'failure);

  let bimap = (onSuccess, onError, xR) =>
    switch (xR) {
    | Ok(x) => onSuccess(x)
    | Error(err) => onError(err)
    };

  let map = (f, result) =>
    switch (result) {
    | Ok(success) => Ok(f(success))
    | Error(err) => Error(err)
    };

  let mapError = (f, result) =>
    switch (result) {
    | Ok(success) => Ok(success)
    | Error(err) => Error(f(err))
    };

  let bind = (f, result) =>
    switch (result) {
    | Ok(success) => f(success)
    | Error(err) => Error(err)
    };

  let iter = (f, result) => map(f, result) |> ignore;

  let apply = (fR, xR) =>
    switch (fR, xR) {
    | (Ok(f), Ok(x)) => Ok(f(x))
    | (Error(err1), Ok(_)) => Error(err1)
    | (Ok(_), Error(err2)) => Error(err2)
    | (Error(err1), Error(_)) => Error(err1)
    };

  let sequence = aListOfResults => {
    let (<*>) = apply;
    let (<!>) = map;
    let cons = (head, tail) => [head, ...tail];
    let consR = (headR, tailR) => cons <!> headR <*> tailR;
    let initialValue = Ok([]);

    // List.foldBack in F#
    List.fold_right(consR, aListOfResults, initialValue);
  };

  let lift2 = (f, x1, x2) => {
    let (<!>) = map;
    let (<*>) = apply;
    f <!> x1 <*> x2;
  };

  let lift3 = (f, x1, x2, x3) => {
    let (<!>) = map;
    let (<*>) = apply;
    f <!> x1 <*> x2 <*> x3;
  };

  let lift4 = (f, x1, x2, x3, x4) => {
    let (<!>) = map;
    let (<*>) = apply;
    f <!> x1 <*> x2 <*> x3 <*> x4;
  };

  let bind2 = (f, x1, x2) => lift2(f, x1, x2) |> bind(id);

  let bind3 = (f, x1, x2, x3) => lift3(f, x1, x2, x3) |> bind(id);

  let isOk =
    fun
    | Ok(_) => true
    | Error(_) => false;

  let isError = xR => !isOk(xR);

  let filter = pred =>
    fun
    | Ok(x) => pred(x)
    | Error(_) => true;

  let ifError = defaultVal =>
    fun
    | Ok(x) => x
    | Error(_) => defaultVal;

  let bindOption = (f, xR) =>
    switch (xR) {
    | Some(x) => f(x) |> map(x => Some(x))
    | None => Ok(None)
    };

  let ofOption = (errorValue, opt) =>
    switch (opt) {
    | Some(x) => Ok(x)
    | None => Error(errorValue)
    };

  let toOption = xR =>
    switch (xR) {
    | Ok(x) => Some(x)
    | Error(_) => None
    };

  let toErrorOption =
    fun
    | Ok(_) => None
    | Error(err) => Some(err);
};

module ResultComputationExpression = {
  /* TODO: Implement something comparable to F# computation expressions*/
};

module Validation = {
  type t('success, 'failure) = Result.t('success, list('failure));

  let apply = (fV: t(_, _), xV: t(_, _)) => {
    switch (fV, xV) {
    | (Ok(f), Ok(x)) => Ok(f(x))
    | (Error(err1), Ok(_)) => Error(err1)
    | (Ok(_), Error(err2)) => Error(err2)
    | (Error(err1), Error(err2)) => Error(err1 @ err2)
    };
  };

  let sequence = (aListOfValidations: list(t(_, _))) => {
    let (<*>) = apply;
    let (<!>) = Result.map;
    let cons = (head, tail) => [head, ...tail];
    let consR = (headR, tailR) => cons <!> headR <*> tailR;
    let initialValue = Ok([]);

    // List.foldBack in F#
    List.fold_right(consR, aListOfValidations, initialValue);
  };

  let ofResult = (xR: t(_, _)) => xR |> Result.mapError(err => [err]);

  let toResult = (xV: t(_, _)): Result.t(_, _) => xV;
};

module Async = {
  /* TODO: Implement something comparable to F# async */
};

module AsyncResult = {
  type t('success, 'failure) = Result.t('success, 'failure);
  /* TODO: Implement something comparable to AsyncResult */
};

module AsyncResultComputationExpression = {
  /* TODO: Implement something comparable to F# computation expressions*/
};