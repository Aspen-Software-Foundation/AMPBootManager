OUTPUT_FORMAT("binary")

SECTIONS {
  . = 0xF000;       /* Set origin address */
  
  .text : {
    *(.text)        /* Pull in code section */
  }

  .data : {
    *(.data)        /* Pull in initialized data */
  }

  .bss : {
    *(.bss)         /* Pull in uninitialized data */
  }
}