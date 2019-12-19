unit DW.iOSapi.Foundation;

interface

uses
  Macapi.ObjectiveC,
  iOSapi.Foundation;

type
  NSLocaleKey = NSString;

  NSLocaleClass = interface(NSObjectClass)
    ['{4597A459-6F9B-49F4-8C80-3F8ED8FDB9D1}']
    {class} function autoupdatingCurrentLocale: NSLocale; cdecl;
    {class} function availableLocaleIdentifiers: NSArray; cdecl;
    {class} function canonicalLanguageIdentifierFromString(&string: NSString): NSString; cdecl;
    {class} function canonicalLocaleIdentifierFromString(&string: NSString): NSString; cdecl;
    {class} function characterDirectionForLanguage(isoLangCode: NSString): NSLocaleLanguageDirection; cdecl;
    {class} function commonISOCurrencyCodes: NSArray; cdecl;
    {class} function componentsFromLocaleIdentifier(&string: NSString): NSDictionary; cdecl;
    {class} function currentLocale: NSLocale; cdecl;
    {class} function ISOCountryCodes: NSArray; cdecl;
    {class} function ISOCurrencyCodes: NSArray; cdecl;
    {class} function ISOLanguageCodes: NSArray; cdecl;
    {class} function lineDirectionForLanguage(isoLangCode: NSString): NSLocaleLanguageDirection; cdecl;
    {class} function localeIdentifierFromComponents(dict: NSDictionary): NSString; cdecl;
    {class} function localeIdentifierFromWindowsLocaleCode(lcid: UInt32): NSString; cdecl;
    {class} function localeWithLocaleIdentifier(ident: NSString): Pointer; cdecl;
    {class} function preferredLanguages: NSArray; cdecl;
    {class} function systemLocale: NSLocale; cdecl;
    {class} function windowsLocaleCodeFromLocaleIdentifier(localeIdentifier: NSString): UInt32; cdecl;
  end;

  NSLocale = interface(NSObject)
    ['{6D772B7D-BE16-4960-A10B-D5BCEE8D3B94}']
    function alternateQuotationBeginDelimiter: NSString; cdecl;
    function alternateQuotationEndDelimiter: NSString; cdecl;
    function calendarIdentifier: NSString; cdecl;
    function collationIdentifier: NSString; cdecl;
    function collatorIdentifier: NSString; cdecl;
    function countryCode: NSString; cdecl;
    function currencyCode: NSString; cdecl;
    function currencySymbol: NSString; cdecl;
    function decimalSeparator: NSString; cdecl;
    [MethodName('displayNameForKey:value:')]
    function displayNameForKey(key: NSLocaleKey; value: Pointer): NSString; cdecl;
    function exemplarCharacterSet: NSCharacterSet; cdecl;
    function groupingSeparator: NSString; cdecl;
    function init: Pointer; cdecl;
    function initWithCoder(aDecoder: NSCoder): Pointer; cdecl;
    function initWithLocaleIdentifier(&string: NSString): Pointer; cdecl;
    function languageCode: NSString; cdecl;
    function localeIdentifier: NSString; cdecl;
    function localizedStringForCalendarIdentifier(calendarIdentifier: NSString): NSString; cdecl;
    function localizedStringForCollationIdentifier(collationIdentifier: NSString): NSString; cdecl;
    function localizedStringForCollatorIdentifier(collatorIdentifier: NSString): NSString; cdecl;
    function localizedStringForCountryCode(countryCode: NSString): NSString; cdecl;
    function localizedStringForCurrencyCode(currencyCode: NSString): NSString; cdecl;
    function localizedStringForLanguageCode(languageCode: NSString): NSString; cdecl;
    function localizedStringForLocaleIdentifier(localeIdentifier: NSString): NSString; cdecl;
    function localizedStringForScriptCode(scriptCode: NSString): NSString; cdecl;
    function localizedStringForVariantCode(variantCode: NSString): NSString; cdecl;
    function objectForKey(key: NSLocaleKey): Pointer; cdecl;
    function quotationBeginDelimiter: NSString; cdecl;
    function quotationEndDelimiter: NSString; cdecl;
    function scriptCode: NSString; cdecl;
    function usesMetricSystem: Boolean; cdecl;
    function variantCode: NSString; cdecl;
  end;
  TNSLocale = class(TOCGenericImport<NSLocaleClass, NSLocale>) end;

implementation

end.
