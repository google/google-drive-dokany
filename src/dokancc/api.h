#ifndef DOKAN_API_H_
#define DOKAN_API_H_

#ifdef DOKANCC_DLL
#ifdef _EXPORTING
#define DOKANCC_API __declspec(dllexport)
#else
#define DOKANCC_API __declspec(dllimport)
#endif  // _EXPORTING
#else  // DOKANCC_DLL
#define DOKANCC_API
#endif  // DOKANCC_DLL

#endif  // DOKAN_API_H_
