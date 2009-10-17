using System;
using System.IO;
using NUnit.Framework;

namespace Pandoc.Tests
{

    [TestFixture]
    public class PandocTests
    {
        private Pandoc.Processor pandoc;

        [SetUp] public void Init() 
        { pandoc = new Pandoc.Processor(); }

        [TearDown] public void Cleanup()
        { pandoc.Dispose(); }

        [Test] public void BasicConversionWorks()
        {
            var reader = new StringReader("Hello, [world](http://world.com)!");
            var writer = new StringWriter();
            pandoc.Process(Format.Markdown, Format.Html, reader, writer);
            Assert.AreEqual("<p\n" +
                            ">Hello, <a href=\"http://world.com\"\n  " + 
                            ">world</a\n  " +
                            ">!</p\n>", 
                            writer.ToString());
        }
    }
} 