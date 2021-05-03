#include <stdlib.h>
#include <stdio.h>
#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>
#include <fcntl.h>

int
main(int argc, char **argv)
{
  struct stat statbuf;
  char *file_data;
  char *data;
  off_t filesize;
  int file_handle;
  if (argc < 2 || argc > 3) {
    fprintf(stderr, "Usage: prepare_financisto input [output]\n");
    exit(1);
  }
  if (stat(argv[1], &statbuf) != 0) {
    fprintf(stderr, "Could not stat %s\n", argv[1]);
    exit(1);
  }
  filesize = statbuf.st_size;
  file_data = (char*)malloc(filesize+1);
  if (file_data == NULL) {
    fprintf(stderr, "Could not allocate %d bytes for file data\n", filesize);
    exit(1);
  }
  file_handle = open(argv[1], O_RDONLY);
  if (read(file_handle, file_data, filesize) != filesize) {
    fprintf(stderr, "Could not read all of file %s\n", argv[1]);
    exit(1);
  }
  close(file_handle);

  for (data = file_data;
       *data & 0x80;
       data++, filesize--) {
    if (filesize == 0) {
      fprintf(stderr, "No non-rubbish bytes found in %s\n", argv[1]);
      exit(1);
    }
  }
  
  file_handle = open(argv[argc-1], O_WRONLY|O_TRUNC|O_CREAT, 0666);
  write(file_handle, data, filesize);
  close(file_handle);
  
  exit(0);
}
