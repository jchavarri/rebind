// From https://github.com/jchavarri/rebind/issues/3
const client = new AWSAppSyncClient({
  url: graphqlEndpoint,
  region: region,
  auth: {
    type: authenticationType,
    apiKey: apiKey
  }
});