open OT_Common_SimpleTypes;

module PersonalName = {
  type t = {
    firstName: String50.t,
    lastName: String50.t,
  };
};

module CustomerInfo = {
  type t = {
    name: PersonalName.t,
    emailAddress: EmailAddress.t,
  };
};

module Address = {
  type t = {
    addressLine1: String50.t,
    addressLine2: option(String50.t),
    addressLine3: option(String50.t),
    addressLine4: option(String50.t),
    city: String50.t,
    zipCode: ZipCode.t,
  };
};