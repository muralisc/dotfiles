import boto3
import argparse

def list_s3_objects(bucket_name, prefix=''):
    """
    List all objects in an S3 bucket with optional prefix filtering
    
    Args:
        bucket_name (str): Name of the S3 bucket
        prefix (str): Optional prefix to filter objects
    """
    # Create S3 client
    s3_client = boto3.client('s3')
    
    # Initialize paginator for handling large number of objects
    paginator = s3_client.get_paginator('list_objects_v2')
    
    page_no=0
    try:
        # Use paginator to handle buckets with many objects
        for page in paginator.paginate(Bucket=bucket_name, Prefix=prefix):
            page_no = page_no + 1
            print(f"Page No : {page_no}")
            if 'Contents' in page:
                for obj in page['Contents']:
                    # print(f"Key: {obj['Key']}, Size: {obj['Size']} bytes, Last Modified: {obj['LastModified']}")
                    print(f"{obj['Key']}")
            else:
                print(f"No objects found in bucket {bucket_name} with prefix {prefix}")
    except Exception as e:
        print(f"Error listing objects: {str(e)}")

def main():
    parser = argparse.ArgumentParser(description='List objects in an S3 bucket')
    parser.add_argument('bucket_name', help='Name of the S3 bucket')
    parser.add_argument('--prefix', default='', help='Optional prefix to filter objects')
    
    args = parser.parse_args()
    list_s3_objects(args.bucket_name, args.prefix)

if __name__ == '__main__':
    main()

