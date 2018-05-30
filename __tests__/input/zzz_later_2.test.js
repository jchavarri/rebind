const companyRecordId = data.records[0].id;
contactsRequestUrl = 'https://api.knack.com/v1/objects/object_2/records';
contactsRequestFilters = [
  {
    'field':'field_19', // Company connection field on Contact object
    'operator':'is',
    'value':[companyRecordId]
  }
];

fullcontactsRequestUrl = contactsRequestUrl + '?filters=' + encodeURIComponent(JSON.stringify(contactsRequestFilters));
