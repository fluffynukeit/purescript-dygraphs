module.exports = function(grunt) {

  "use strict";

  grunt.initConfig({

    libFiles: [
      "src/**/*.purs",
      "bower_components/purescript-*/src/**/*.purs",
    ],

    clean: ["tmp", "output"],

    pscMake: ["<%=libFiles%>"],
    dotPsci: ["<%=libFiles%>"],
    pscDocs: {
        readme: {
            src: "src/**/*.purs",
            dest: "docs/Module.md"
        }
    },

    psc: {
      exampleTest: {
        options: { main: "Examples", modules: "Examples" },
        src: ["<%=libFiles%>", "examples/examples.purs"],
        dest: "tmp/Examples.js"
      }
    }
  }
  );

  grunt.loadNpmTasks("grunt-contrib-clean");
  grunt.loadNpmTasks("grunt-purescript");

  grunt.registerTask("make", ["pscMake", "dotPsci", "pscDocs"]);
  grunt.registerTask("default", ["clean", "make", "examples"]);
  grunt.registerTask("examples", ["psc"]);
};

