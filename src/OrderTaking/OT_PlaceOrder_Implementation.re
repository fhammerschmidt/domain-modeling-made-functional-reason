open OT_Common.SimpleTypes;
open OT_Common.CompoundTypes;
open OT_PlaceOrder_PublicTypes;
open ResultUtils;

type checkProductCodexists = ProductCode.t => bool;

module AddressValidationError = {
  type t =
    | InvalidFormat
    | AddressNotFound;
};

module CheckedAddress = {
  type t =
    | CheckedAddress(UnvalidatedAddress.t);
};

type checkAddressExists = UnvalidatedAddress.t => CheckedAddress.t;

module ValidatedOrderLine = {
  type t = {
    orderLineId: OrderLineId.t,
    productCode: ProductCode.t,
    quantity: OrderQuantity.t,
  };
};

module ValidatedOrder = {
  type t = {
    orderId: OrderId.t,
    customerInfo: CustomerInfo.t,
    shippingAddress: Address.t,
    billingAddress: Address.t,
    lines: list(ValidatedOrderLine.t),
  };
};

type validateOrder =
  (checkProductCodexists, checkAddressExists, UnvalidatedOrder.t) =>
  AsyncResult.t(ValidatedOrder.t, ValidationError.t);

type getProductPrice = ProductCode.t => Price.t;

type priceOrder =
  (getProductPrice, ValidatedOrder.t) =>
  Result.t(PricedOrder.t, PricingError.t);

module HtmlString = {
  type t =
    | HtmlString(string);
};

module OrderAcknowledgment = {
  type t = {
    emailAddress: EmailAddress.t,
    letter: HtmlString.t,
  };
};

type createOrderAcknowledgementLetter = PricedOrder.t => HtmlString.t;

module SendResult = {
  type t =
    | Sent
    | NotSent;
};

type sendOrderAcknowledgement = OrderAcknowledgment.t => SendResult.t;

type acknowledgeOrder =
  (
    createOrderAcknowledgementLetter,
    sendOrderAcknowledgement,
    PricedOrder.t
  ) =>
  option(OrderAcknowledgmentSent.t);

type createEvents =
  (PricedOrder.t, option(OrderAcknowledgmentSent.t)) =>
  list(PlaceOrderEvent.t);