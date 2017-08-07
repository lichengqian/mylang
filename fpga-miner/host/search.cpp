// Copyright (C) 2013-2016 Altera Corporation, San Jose, California, USA. All rights reserved.
// Permission is hereby granted, free of charge, to any person obtaining a copy of this
// software and associated documentation files (the "Software"), to deal in the Software
// without restriction, including without limitation the rights to use, copy, modify, merge,
// publish, distribute, sublicense, and/or sell copies of the Software, and to permit persons to
// whom the Software is furnished to do so, subject to the following conditions:
// The above copyright notice and this permission notice shall be included in all copies or
// substantial portions of the Software.
// 
// THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
// EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES
// OF MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
// NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
// HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
// WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
// FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR
// OTHER DEALINGS IN THE SOFTWARE.
// 
// This agreement shall be governed in all respects by the laws of the State of California and
// by the laws of the United States of America.

///////////////////////////////////////////////////////////////////////////////////
// This host program executes a vector addition kernel to perform:
//  C = A + B
// where A, B and C are vectors with N elements.
//
// This host program supports partitioning the problem across multiple OpenCL
// devices if available. If there are M available devices, the problem is
// divided so that each device operates on N/M points. The host program
// assumes that all devices are of the same type (that is, the same binary can
// be used), but the code can be generalized to support different device types
// easily.
//
// Verification is performed against the same computation on the host CPU.
///////////////////////////////////////////////////////////////////////////////////

#include <stdio.h>
#include <stdlib.h>
#include <math.h>
#include "CL/opencl.h"
#include "AOCLUtils/aocl_utils.h"

using namespace aocl_utils;

// OpenCL runtime configuration
cl_platform_id platform = NULL;
unsigned num_devices = 0;
scoped_array<cl_device_id> device; // num_devices elements
cl_context context = NULL;
scoped_array<cl_command_queue> queue; // num_devices elements
cl_program program = NULL;
scoped_array<cl_kernel> kernel; // num_devices elements

// kernel args
scoped_array<cl_mem> g_output_buf; // num_devices elements
scoped_array<cl_mem> g_header_buf; // num_devices elements
scoped_array<cl_mem> g_dag_buf; // num_devices elements

scoped_array<scoped_aligned_ptr<uint16_t> > g_output; // num_devices elements
scoped_array<scoped_aligned_ptr<uint16_t> > g_header; // num_devices elements
scoped_array<scoped_aligned_ptr<uint16_t> > g_dag; // num_devices elements

scoped_array<unsigned> n_per_device; // num_devices elements

const char *program_name = "ethash_b";
const char *kernel_name = "search";

// Problem data.
unsigned N = 1000;
unsigned DAG_SIZE = N; // problem size

// Function prototypes
bool init_opencl();
void init_problem();
void run();
void cleanup();

// Entry point.
int main(int argc, char **argv) {
  Options options(argc, argv);

  // Optional argument to specify the problem size.
  if(options.has("n")) {
    N = options.get<unsigned>("n");
  }

  // Initialize OpenCL.
  if(!init_opencl()) {
    return -1;
  }

  // Initialize the problem data.
  // Requires the number of devices to be known.
  init_problem();

  // Run the kernel.
  run();

  // Free the resources allocated
  cleanup();

  return 0;
}

/////// HELPER FUNCTIONS ///////

// Randomly generate a floating-point number between -10 and 10.
uint16_t rand_float() {
  return uint16_t(rand()) % 100;
}

// Initializes the OpenCL objects.
bool init_opencl() {
  cl_int status;

  printf("Initializing OpenCL\n");

  if(!setCwdToExeDir()) {
    return false;
  }

  // Get the OpenCL platform.
  platform = findPlatform("Intel(R) FPGA");
  if(platform == NULL) {
    printf("ERROR: Unable to find Intel(R) FPGA OpenCL platform.\n");
    return false;
  }

  // Query the available OpenCL device.
  device.reset(getDevices(platform, CL_DEVICE_TYPE_ALL, &num_devices));
  printf("Platform: %s\n", getPlatformName(platform).c_str());
  printf("Using %d device(s)\n", num_devices);
  for(unsigned i = 0; i < num_devices; ++i) {
    printf("  %s\n", getDeviceName(device[i]).c_str());
  }

  // Create the context.
  context = clCreateContext(NULL, num_devices, device, &oclContextCallback, NULL, &status);
  checkError(status, "Failed to create context");

  // Create the program for all device. Use the first device as the
  // representative device (assuming all device are of the same type).
  std::string binary_file = getBoardBinaryFile(program_name, device[0]);
  printf("Using AOCX: %s\n", binary_file.c_str());
  program = createProgramFromBinary(context, binary_file.c_str(), device, num_devices);

  // Build the program that was just created.
  status = clBuildProgram(program, 0, NULL, "", NULL, NULL);
  checkError(status, "Failed to build program");

  // Create per-device objects.
  queue.reset(num_devices);
  kernel.reset(num_devices);
  n_per_device.reset(num_devices);

  g_output_buf.reset(num_devices);
  g_header_buf.reset(num_devices);
  g_dag_buf.reset(num_devices);


  for(unsigned i = 0; i < num_devices; ++i) {
    // Command queue.
    queue[i] = clCreateCommandQueue(context, device[i], CL_QUEUE_PROFILING_ENABLE, &status);
    checkError(status, "Failed to create command queue");

    // Kernel.
    kernel[i] = clCreateKernel(program, kernel_name, &status);
    checkError(status, "Failed to create kernel");

    // Determine the number of elements processed by this device.
    n_per_device[i] = N;

    printf("Creating g_output\n");
    // Output buffer.
    g_output_buf[i] = clCreateBuffer(context, CL_MEM_WRITE_ONLY, 
        256 * sizeof(uint16_t), NULL, &status);
    checkError(status, "Failed to create buffer for output");

    printf("Creating g_header\n");
    // Input buffers.
    g_header_buf[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, 
        128 * sizeof(uint16_t), NULL, &status);
    checkError(status, "Failed to create buffer for input A");

    printf("Creating g_dag\n");
    g_dag_buf[i] = clCreateBuffer(context, CL_MEM_READ_ONLY, 
        DAG_SIZE * sizeof(uint16_t), NULL, &status);
    checkError(status, "Failed to create buffer for input A");

  }

  return true;
}

// Initialize the data for the problem. Requires num_devices to be known.
void init_problem() {
  if(num_devices == 0) {
    checkError(-1, "No devices");
  }

  printf("Initializing problem %d\n", num_devices);

  g_output.reset(num_devices);
  g_header.reset(num_devices);
  g_dag.reset(num_devices);

  // Generate input vectors A and B and the reference output consisting
  // of a total of N elements.
  // We create separate arrays for each device so that each device has an
  // aligned buffer.
  for(unsigned i = 0; i < num_devices; ++i) {
    g_output[i].reset(256);
    g_header[i].reset(128);
    g_dag[i].reset(DAG_SIZE);

    printf("Initializing g_header\n");
    for(unsigned j = 0; j < 128; ++j) {
      g_header[i][j] = rand_float();
    }

    printf("Initializing g_dag\n");
    for(unsigned j = 0; j < DAG_SIZE; ++j) {
      g_dag[i][j] = rand_float();
    }
  }
}

void run() {
  cl_int status;

  printf("runing problem\n");
  const double start_time = getCurrentTimestamp();

  // Launch the problem for each device.
  scoped_array<cl_event> kernel_event(num_devices);
  scoped_array<cl_event> finish_event(num_devices);

  for(unsigned i = 0; i < num_devices; ++i) {


    // Transfer inputs to each device. Each of the host buffers supplied to
    // clEnqueueWriteBuffer here is already aligned to ensure that DMA is used
    // for the host-to-device transfer.
    cl_event write_event[2];

    printf("Writing g_header\n");
    status = clEnqueueWriteBuffer(queue[i], g_header_buf[i], CL_FALSE,
        0, 128 * sizeof(uint16_t), g_dag[i], 0, NULL, &write_event[0]);
    checkError(status, "Failed to transfer g_header");

    printf("Writing g_dag\n");
    status = clEnqueueWriteBuffer(queue[i], g_dag_buf[i], CL_FALSE,
        0, DAG_SIZE * sizeof(uint16_t), g_dag[i], 0, NULL, &write_event[1]);
    checkError(status, "Failed to transfer g_dag");


    // Set kernel arguments.
    unsigned argi = 0;
    status = clSetKernelArg(kernel[i], argi++, sizeof(cl_mem), &g_output_buf[i]);
    checkError(status, "Failed to set argument %d", argi - 1);

    status = clSetKernelArg(kernel[i], argi++, sizeof(cl_mem), &g_header_buf[i]);
    checkError(status, "Failed to set argument %d", argi - 1);

    status = clSetKernelArg(kernel[i], argi++, sizeof(cl_mem), &g_dag_buf[i]);
    checkError(status, "Failed to set argument %d", argi - 1);

    uint32_t dag_size = 100;
    status = clSetKernelArg(kernel[i], argi++, sizeof(dag_size), &dag_size);
    checkError(status, "Failed to set argument %d", argi - 1);

    uint64_t start_nonce = 0;
    status = clSetKernelArg(kernel[i], argi++, sizeof(start_nonce), &start_nonce);
    checkError(status, "Failed to set argument %d", argi - 1);

    uint64_t targer = 0xFFFFFFFF;
    status = clSetKernelArg(kernel[i], argi++, sizeof(targer), &targer);
    checkError(status, "Failed to set argument %d", argi - 1);

    unsigned arg_isolate = 1;
    status = clSetKernelArg(kernel[i], argi++, sizeof(arg_isolate), &arg_isolate);
    checkError(status, "Failed to set argument %d", argi - 1);

    // Enqueue kernel.
    // Use a global work size corresponding to the number of elements to add
    // for this device.
    //
    // We don't specify a local work size and let the runtime choose
    // (it'll choose to use one work-group with the same size as the global
    // work-size).
    //
    // Events are used to ensure that the kernel is not launched until
    // the writes to the input buffers have completed.
    const size_t global_work_size = 256 * 10000;
    printf("Launching for device %d (%zd elements)\n", i, global_work_size);


    status = clEnqueueNDRangeKernel(queue[i], kernel[i], 1, NULL,
        &global_work_size, NULL, 2, write_event, &kernel_event[i]);
    checkError(status, "Failed to launch kernel");


    // Read the result. This the final operation.
    status = clEnqueueReadBuffer(queue[i], g_output_buf[i], CL_FALSE,
        0, 256 * sizeof(uint16_t), g_output[i], 1, &kernel_event[i], &finish_event[i]);

    // Release local events.
    clReleaseEvent(write_event[0]);
    clReleaseEvent(write_event[1]);

  }

  // Wait for all devices to finish.
  clWaitForEvents(num_devices, finish_event);

  const double end_time = getCurrentTimestamp();

  // Wall-clock time taken.
  printf("\nTime: %0.3f ms\n", (end_time - start_time) * 1e3);

  // Get kernel times using the OpenCL event profiling API.
  for(unsigned i = 0; i < num_devices; ++i) {
    cl_ulong time_ns = getStartEndTime(kernel_event[i]);
    printf("Kernel time (device %d): %0.3f ms\n", i, double(time_ns) * 1e-6);
  }

  // Release all events.
  for(unsigned i = 0; i < num_devices; ++i) {
    clReleaseEvent(kernel_event[i]);
    clReleaseEvent(finish_event[i]);
  }

  // Verify results.
  for(unsigned i = 0; i < num_devices; ++i) {
    for(unsigned j = 0; j < n_per_device[i]; ++j) {
        printf("%d %d",j, g_output[i][j]);
	if (j % 16 == 0)
	    printf("\n");
    }
  }
}

// Free the resources allocated during initialization
void cleanup() {
  for(unsigned i = 0; i < num_devices; ++i) {
    if(kernel && kernel[i]) {
      clReleaseKernel(kernel[i]);
    }
    if(queue && queue[i]) {
      clReleaseCommandQueue(queue[i]);
    }

    if(g_header_buf && g_header_buf[i]) {
      clReleaseMemObject(g_header_buf[i]);
    }
    if(g_dag_buf && g_dag_buf[i]) {
      clReleaseMemObject(g_dag_buf[i]);
    }
    if(g_output_buf && g_output_buf[i]) {
      clReleaseMemObject(g_output_buf[i]);
    }
  }

  if(program) {
    clReleaseProgram(program);
  }
  if(context) {
    clReleaseContext(context);
  }
}

