#include <string.h>
#include <stdlib.h>
#include "shapefil.h"
#include "../nkf/libnkf.h"
//==============================================================
//
//==============================================================
SHPHandle hSHP;
DBFHandle hDBF;
//==============================================================
//
//==============================================================
void c_shpopen_( char *file ){
  hSHP = SHPOpen( file, "rb" );

  if( hSHP == NULL ){
    fprintf( stderr, "Unable to open: %s\n", file );
    exit( EXIT_FAILURE );
  }
}
//==============================================================
//
//==============================================================
void c_shpclose_( ){
  SHPClose( hSHP );

  hSHP = NULL;
}
//==============================================================
//
//==============================================================
void c_shpgetinfo_( int *nEntities, 
                    int *type, char *typeName, int *typeNameLength, 
                    double *adfMinBound, double *adfMaxBound ){
  *nEntities = hSHP->nRecords;

  *type = hSHP->nShapeType;

  const char * typeName_ = SHPTypeName(*type);
  *typeNameLength = strlen(typeName_);

  int i;
  for( i = 0; i < *typeNameLength; i++ ){
    typeName[i] = typeName_[i];
  }

  for( i = 0; i < 4; i++ ){
    adfMinBound[i] = hSHP->adBoundsMin[i];
    adfMaxBound[i] = hSHP->adBoundsMax[i];
  }
}
//==============================================================
//
//==============================================================
void c_shpgetentityinfo_(
    int *iEntity, 
    int *nVertices, int *nParts, int *bMeasureIsUsed, 
    double *dfXMin, double *dfXMax, 
    double *dfYMin, double *dfYMax, 
    double *dfZMin, double *dfZMax, 
    double *dfMMin, double *dfMMax,
    int *stat
  ){

  SHPObject *psShape;
  psShape = SHPReadObject( hSHP, *iEntity );

  *stat = 0;

  if( psShape == NULL ){
    fprintf( stderr, "Unable to read shape %d, terminating object reading.\n", *iEntity );
    *stat = 1;
    return;
  }

  *nVertices      = psShape->nVertices;
  *nParts         = psShape->nParts;
  *bMeasureIsUsed = psShape->bMeasureIsUsed;
  *dfXMin = psShape->dfXMin;
  *dfXMax = psShape->dfXMax;
  *dfYMin = psShape->dfYMin;
  *dfYMax = psShape->dfYMax;
  *dfZMin = psShape->dfZMin;
  *dfZMax = psShape->dfZMax;
  *dfMMin = psShape->dfMMin;
  *dfMMax = psShape->dfMMax;

  SHPDestroyObject( psShape );
}
//==============================================================
//
//==============================================================
void c_shpgetpanpartstart_( int *iEntity, int *panPartStart, int *stat ){
  int iPart;

  SHPObject *psShape;
  psShape = SHPReadObject( hSHP, *iEntity );

  *stat = 0;

  if( psShape == NULL ){
    fprintf( stderr, "Unable to read shape %d, terminating object reading.\n", *iEntity );
    *stat = 1;
    return;
  }

  for( iPart = 0; iPart < psShape->nParts; iPart++ ){
    panPartStart[iPart] = psShape->panPartStart[iPart];
  }

  SHPDestroyObject( psShape );
}
//==============================================================
//
//==============================================================
void c_shpgetdata_( int *iEntity, 
                    double *padfX, double *padfY, double *padfZ, double *padfM,
                    int *stat ){
  int iVertex;

  SHPObject *psShape;
  psShape = SHPReadObject( hSHP, *iEntity );

  *stat = 0;

  if( psShape == NULL ){
    fprintf( stderr, "Unable to read shape %d, terminating object reading.\n", *iEntity );
    *stat = 1;
    return;
  }

  if( psShape->bMeasureIsUsed ){
    for( iVertex = 0; iVertex < psShape->nVertices; iVertex++ ){
      padfX[iVertex] = psShape->padfX[iVertex];
      padfY[iVertex] = psShape->padfY[iVertex];
      padfZ[iVertex] = psShape->padfZ[iVertex];
      padfM[iVertex] = psShape->padfM[iVertex];
    }
  } else {
    for( iVertex = 0; iVertex < psShape->nVertices; iVertex++ ){
      padfX[iVertex] = psShape->padfX[iVertex];
      padfY[iVertex] = psShape->padfY[iVertex];
      padfZ[iVertex] = psShape->padfZ[iVertex];
    }
  }

  SHPDestroyObject( psShape );
}
//==============================================================
//
//==============================================================
void c_dbfopen_( char *file, int *stat ){
  hDBF = DBFOpen( file, "rb" );
  *stat = 0;

  if( hDBF == NULL ){
    fprintf( stderr, "Unable to open: %s\n", file );
    *stat = 1;
    return;
  }
}
//==============================================================
//
//==============================================================
void c_dbfclose_( ){
  DBFClose( hDBF );

  hDBF = NULL;
}
//==============================================================
//
//==============================================================
void c_dbfgetfieldcount_( int *nFields ){
  *nFields = DBFGetFieldCount(hDBF);
}
//==============================================================
//
//==============================================================
void c_dbfgetrecordcount_( int *nRecords ){
  *nRecords = DBFGetRecordCount(hDBF);
}
//==============================================================
//
//==============================================================
void c_dbfgetfieldinfo_( 
        int *iField, 
        char *chNativeType, char *pszTypeName, int *typeNameLength, 
        char *szTitle, int *titleLength, int *nWidth, int *nDecimals ){

  DBFFieldType eType;
  const char * typeName;
  const char * title;
  int i;

  *chNativeType = DBFGetNativeFieldType( hDBF, *iField );

  eType = DBFGetFieldInfo( hDBF, *iField, szTitle, nWidth, nDecimals );
  if( eType == FTString )
    typeName = "String";
  else if( eType == FTInteger )
    typeName = "Integer";
  else if( eType == FTDouble )
    typeName = "Double";
  else if( eType == FTInvalid )
    typeName = "Invalid";

  *typeNameLength = strlen(typeName);

  for( i = 0; i < *typeNameLength; i++ ){
    pszTypeName[i] = typeName[i];
  }

  title = (char *)nkf_guess_convert( szTitle );
  *titleLength = strlen(title);

  for( i = 0; i < *titleLength; i++ ){
    szTitle[i] = title[i];
  }
}
//==============================================================
//
//==============================================================
void c_dbfgetrecord_( int *iRecord, int *iField, char *typeChar,
                      int *recslen, char *recs, int *reci, double *recd ){
  const char * s;

  *recslen = 0;
  recs[0] = '\0';
  *reci = 0;
  *recd = 0.0;

  switch( *typeChar ){

    case 'C':
      s = nkf_guess_convert( (char *)DBFReadStringAttribute( hDBF, *iRecord, *iField ) );

      *recslen = strlen(s);
      int i;
      for( i = 0; i < *recslen; i++ ){
        recs[i] = s[i];
      }
      break;

    case 'N':
      //printf("Field %d (N) %d\n", *iField, DBFReadIntegerAttribute( hDBF, *iRecord, *iField ) );
      *reci = DBFReadIntegerAttribute( hDBF, *iRecord, *iField );
      break;

    case 'D':
      //printf("Field %d (D) %f\n", *iField, DBFReadDoubleAttribute( hDBF, *iRecord, *iField) );
      *recd = DBFReadIntegerAttribute( hDBF, *iRecord, *iField );
      break;

    default:
      break;
  }
}
