import boto3
import os
import logging
import sys
import datetime

# global/config variables
__author__ = 'jhensleigh@usgs.gov'
__version__ = 0.0
SCRIPT_ALIAS = os.path.splitext(os.path.basename(__file__))[0]
START_TIME = datetime.datetime.now()
LOGGING = True
LOG_LEVEL = logging.INFO
LOG_TIME_FORMAT = '%Y%m%d%H%M%S%f'


# script config/defaultsx
BUCKET_NAME = 'sbsc-upload-data'

def execute(bucket_name,
            output_directory = os.getcwd(),
            file_size_limit_in_bytes = 500000000000):
  ''' loops over s3 bucket and downloads all files with a size less than or equal to the file size limit

      keyword arguments:

      bucket_name -- the name of the bucket to loop over
      output_directory -- directory to write the files to (defaults to the executing directory of the script)
      file_size_limit_in_bytes -- file size limit of files to download in bytes (defaults to 500 GB)



  '''

  s3 = boto3.resource('s3')
  bucket = s3.Bucket(BUCKET_NAME)

  for obj in bucket.objects.all():  
  
    obj = s3.Object(BUCKET_NAME, obj.key)

    if obj.content_length <= file_size_limit_in_bytes:

      try:
        
        object_name = os.path.basename(obj.key)
        logging.info('DOWNLOADING: %s' % object_name)
        obj.download_file(os.path.join(output_directory,object_name))
        logging.info('DOWNLOAD COMPLETE: %s' % object_name)

      except Exception as ex:

        logging.error(ex.args, exc_info=True)


if __name__ == '__main__':

  # directory of python file
  executing_directory = os.path.dirname(os.path.realpath(__file__))
  timestamp = START_TIME.strftime(LOG_TIME_FORMAT)

  # check that the log directory exists
  log_directory = os.path.join(executing_directory,
                               'resources%slog' % os.sep)
  if not os.path.isdir(log_directory):

    os.makedirs(log_directory)

  log_file_path = os.path.join(log_directory,
                               '%s_%s.log' % (SCRIPT_ALIAS,
                                              timestamp))

  print 'LOGGING AT: %s' % log_file_path

  # setup log
  logging.basicConfig(filename = log_file_path,
                      level = LOG_LEVEL)
  logging.info('START TIME: %s' % START_TIME)


  try:

    # execute
    execute(BUCKET_NAME)

  except Exception as ex:

    logging.error(ex.args, exc_info=True)

  finally:

    END_TIME = datetime.datetime.now()
    DURATION = END_TIME - START_TIME
    logging.info('END_TIME: %s' % END_TIME)
    logging.info('DURATION: %s' % DURATION)
