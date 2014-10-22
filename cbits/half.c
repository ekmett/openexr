#include <ImfCRgbaFile.h>

ImfHalf c_floatToHalf (float f) {
  ImfHalf h;
  ImfFloatToHalf(f,&h);
  return h;
}
