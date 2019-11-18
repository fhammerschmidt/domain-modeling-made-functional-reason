open OT_Common.SimpleTypes;
open OT_Common.CompoundTypes;

module UnvalidatedCustomerInfo = {
  type t = {
    firstName: string,
    lastName: string,
    emailAddress: string,
  };
};

module UnvalidatedAddress = {
  type t = {
    addressLine1: string,
    addressLine2: string,
    addressLine3: string,
    addressLine4: string,
    city: string,
    zipCode: string,
  };
};

module UnvalidatedOrderLine = {
  type t = {
    orderLineId: string,
    productCode: string,
    quantity: float,
  };
};

module UnvalidatedOrder = {
  type t = {
    orderId: string,
    customerInfo: UnvalidatedCustomerInfo.t,
    shippingAddress: UnvalidatedAddress.t,
    billingAddress: UnvalidatedAddress.t,
    lines: list(UnvalidatedOrderLine.t),
  };
};

module OrderAcknowledgmentSent = {
  type t = {
    orderId: OrderId.t,
    emailAddress: EmailAddress.t,
  };
};

module PricedOrderLine = {
  type t = {
    orderLineId: OrderLineId.t,
    productCode: ProductCode.t,
    quantity: OrderQuantity.t,
    linePrice: Price.t,
  };
};

module PricedOrder = {
  type t = {
    orderId: OrderId.t,
    customerInfo: CustomerInfo.t,
    shippingAddress: Address.t,
    billingAddress: Address.t,
    amountToBill: BillingAmount.t,
    lines: list(PricedOrderLine.t),
  };
};

module OrderPlaced = PricedOrder;

module BillableOrderPlaced = {
  type t = {
    orderId: OrderId.t,
    billingAddress: Address.t,
    amountToBill: BillingAmount.t,
  };
};

module PlaceOrderEvent = {
  type t =
    | OrderPlaced(OrderPlaced.t)
    | BillableOrderPlaced(BillableOrderPlaced.t)
    | OrderAcknowledgmentSent(OrderAcknowledgmentSent.t);
};

module ValidationError = {
  type t =
    | ValidationError(string);
};

module PricingError = {
  type t =
    | PricingError(string);
};

module ServiceInfo = {
  type t = {
    name: string,
    endpoint: string,
  };
};

module RemoteServiceError = {
  type t = {
    service: ServiceInfo.t,
    exception_: string // System.Exception
  };
};

module PlaceOrderError = {
  type t =
    | Validation(ValidationError.t)
    | Pricing(PricingError.t)
    | RemoteService(RemoteServiceError.t);
};

module PlaceOrder = {
  type t =
    UnvalidatedOrder.t =>
    Belt.Result.t(list(PlaceOrderEvent.t), PlaceOrderError.t);
};