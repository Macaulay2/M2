


#ifdef __cplusplus
extern "C" {
#endif


  struct M2File;

  int M2File_GetThreadMode(struct M2File* file);

  void M2File_SetThreadMode(struct M2File* file, int threadMode);

  struct M2File*  M2File_New(stdio0_fileOutputSyncState fileUnsyncState); 
  stdio0_fileOutputSyncState M2File_UnsyncState(struct M2File* file);
  stdio0_fileOutputSyncState M2File_GetState(struct M2File* file);
  void M2File_ReleaseState(struct M2File* file);
  void M2File_StartInput(struct M2File* file);
  void M2File_EndInput(struct M2File* file);
  void M2File_StartOutput(struct M2File* file);
  void M2File_EndOutput(struct M2File* file);


#ifdef __cplusplus
};
#endif
