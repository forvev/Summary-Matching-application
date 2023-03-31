---------------------------------------------------
**To run the application**
1. Download the jar file from the latest release
2. Run the command:

  **java -jar downloaded_jar.jar -i jar_of_the_android_app.jar -s summaries_folder -o output_folder**
  
If all of the files are in the same directory there is no need to add the absolute path. Otherwise, it is mandatory.

As an output, you should get a folder "xml_as_json" and two files (match dependencies and match summaries).


---------------------------------------------------
**To run the docker**

Once you have downloaded the project you will find there the Dockerfile file. Inside the same directory run this command to build an image:
 
 **sudo docker build .**
 
It will create an image that you can run in the following way:

  **sudo docker run --rm -v /path_where_you want_to_store_output/summary-matching:/output -it your_image -i /usr/cpa/src/main/resources/jar-files/classes.jar -s /usr/cpa/src/main/resources/summary-files -o /output**
  
  The result will be stored inside the summary-matching folder. In order to change paths you would need to change the Dockerfile.


---------------------------------------------------
**Working of the application**

Once the files are passed to the application, it carries out the analysis process. If the summary files are represented in XML notation they will be translated into the JSON representation.
After that the application checks if the summaries match the classes inside the android application. If all of the methods are included inside the specific class in the android application it means that there is a match between the summary and the class.
Once we have a list of matched summaries the program checks the dependencies between them. If a class A with a matched summary implements a class B with a matched summary too, it means that there is a dependency between them. More nested cases are also considered. Therefore, if there is a class C with a matched summary, and this class implements a class A it means that there is a dependency between B and C.
The result of this process will be stored inside the output as well as the output of the summaries.
