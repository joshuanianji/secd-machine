import { defineConfig } from "vite";
import elmPlugin from "vite-plugin-elm";
import plainText from "vite-plugin-plain-text";

export default defineConfig({
  plugins: [elmPlugin(), plainText(/.+\.lisp$/)],
});
