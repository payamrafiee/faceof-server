module.exports =
/******/ (function(modules) { // webpackBootstrap
/******/ 	// The module cache
/******/ 	var installedModules = {};
/******/
/******/ 	// The require function
/******/ 	function __webpack_require__(moduleId) {
/******/
/******/ 		// Check if module is in cache
/******/ 		if(installedModules[moduleId]) {
/******/ 			return installedModules[moduleId].exports;
/******/ 		}
/******/ 		// Create a new module (and put it into the cache)
/******/ 		var module = installedModules[moduleId] = {
/******/ 			i: moduleId,
/******/ 			l: false,
/******/ 			exports: {}
/******/ 		};
/******/
/******/ 		// Execute the module function
/******/ 		modules[moduleId].call(module.exports, module, module.exports, __webpack_require__);
/******/
/******/ 		// Flag the module as loaded
/******/ 		module.l = true;
/******/
/******/ 		// Return the exports of the module
/******/ 		return module.exports;
/******/ 	}
/******/
/******/
/******/ 	// expose the modules object (__webpack_modules__)
/******/ 	__webpack_require__.m = modules;
/******/
/******/ 	// expose the module cache
/******/ 	__webpack_require__.c = installedModules;
/******/
/******/ 	// define getter function for harmony exports
/******/ 	__webpack_require__.d = function(exports, name, getter) {
/******/ 		if(!__webpack_require__.o(exports, name)) {
/******/ 			Object.defineProperty(exports, name, { enumerable: true, get: getter });
/******/ 		}
/******/ 	};
/******/
/******/ 	// define __esModule on exports
/******/ 	__webpack_require__.r = function(exports) {
/******/ 		if(typeof Symbol !== 'undefined' && Symbol.toStringTag) {
/******/ 			Object.defineProperty(exports, Symbol.toStringTag, { value: 'Module' });
/******/ 		}
/******/ 		Object.defineProperty(exports, '__esModule', { value: true });
/******/ 	};
/******/
/******/ 	// create a fake namespace object
/******/ 	// mode & 1: value is a module id, require it
/******/ 	// mode & 2: merge all properties of value into the ns
/******/ 	// mode & 4: return value when already ns object
/******/ 	// mode & 8|1: behave like require
/******/ 	__webpack_require__.t = function(value, mode) {
/******/ 		if(mode & 1) value = __webpack_require__(value);
/******/ 		if(mode & 8) return value;
/******/ 		if((mode & 4) && typeof value === 'object' && value && value.__esModule) return value;
/******/ 		var ns = Object.create(null);
/******/ 		__webpack_require__.r(ns);
/******/ 		Object.defineProperty(ns, 'default', { enumerable: true, value: value });
/******/ 		if(mode & 2 && typeof value != 'string') for(var key in value) __webpack_require__.d(ns, key, function(key) { return value[key]; }.bind(null, key));
/******/ 		return ns;
/******/ 	};
/******/
/******/ 	// getDefaultExport function for compatibility with non-harmony modules
/******/ 	__webpack_require__.n = function(module) {
/******/ 		var getter = module && module.__esModule ?
/******/ 			function getDefault() { return module['default']; } :
/******/ 			function getModuleExports() { return module; };
/******/ 		__webpack_require__.d(getter, 'a', getter);
/******/ 		return getter;
/******/ 	};
/******/
/******/ 	// Object.prototype.hasOwnProperty.call
/******/ 	__webpack_require__.o = function(object, property) { return Object.prototype.hasOwnProperty.call(object, property); };
/******/
/******/ 	// __webpack_public_path__
/******/ 	__webpack_require__.p = "";
/******/
/******/
/******/ 	// Load entry module and return exports
/******/ 	return __webpack_require__(__webpack_require__.s = "./src/index.js");
/******/ })
/************************************************************************/
/******/ ({

/***/ "./node_modules/bcrypt-nodejs/bCrypt.js":
/*!**********************************************!*\
  !*** ./node_modules/bcrypt-nodejs/bCrypt.js ***!
  \**********************************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

eval("var crypto = __webpack_require__(/*! crypto */ \"crypto\");\r\n\r\nvar BCRYPT_SALT_LEN = 16;\r\n\r\nvar GENSALT_DEFAULT_LOG2_ROUNDS = 10;\r\nvar BLOWFISH_NUM_ROUNDS = 16;\r\n\r\nvar MAX_EXECUTION_TIME = 100;\r\nvar P_orig = [0x243f6a88, 0x85a308d3, 0x13198a2e, 0x03707344, 0xa4093822,\r\n\t\t0x299f31d0, 0x082efa98, 0xec4e6c89, 0x452821e6, 0x38d01377,\r\n\t\t0xbe5466cf, 0x34e90c6c, 0xc0ac29b7, 0xc97c50dd, 0x3f84d5b5,\r\n\t\t0xb5470917, 0x9216d5d9, 0x8979fb1b];\r\nvar S_orig = [0xd1310ba6, 0x98dfb5ac, 0x2ffd72db, 0xd01adfb7, 0xb8e1afed,\r\n\t\t0x6a267e96, 0xba7c9045, 0xf12c7f99, 0x24a19947, 0xb3916cf7,\r\n\t\t0x0801f2e2, 0x858efc16, 0x636920d8, 0x71574e69, 0xa458fea3,\r\n\t\t0xf4933d7e, 0x0d95748f, 0x728eb658, 0x718bcd58, 0x82154aee,\r\n\t\t0x7b54a41d, 0xc25a59b5, 0x9c30d539, 0x2af26013, 0xc5d1b023,\r\n\t\t0x286085f0, 0xca417918, 0xb8db38ef, 0x8e79dcb0, 0x603a180e,\r\n\t\t0x6c9e0e8b, 0xb01e8a3e, 0xd71577c1, 0xbd314b27, 0x78af2fda,\r\n\t\t0x55605c60, 0xe65525f3, 0xaa55ab94, 0x57489862, 0x63e81440,\r\n\t\t0x55ca396a, 0x2aab10b6, 0xb4cc5c34, 0x1141e8ce, 0xa15486af,\r\n\t\t0x7c72e993, 0xb3ee1411, 0x636fbc2a, 0x2ba9c55d, 0x741831f6,\r\n\t\t0xce5c3e16, 0x9b87931e, 0xafd6ba33, 0x6c24cf5c, 0x7a325381,\r\n\t\t0x28958677, 0x3b8f4898, 0x6b4bb9af, 0xc4bfe81b, 0x66282193,\r\n\t\t0x61d809cc, 0xfb21a991, 0x487cac60, 0x5dec8032, 0xef845d5d,\r\n\t\t0xe98575b1, 0xdc262302, 0xeb651b88, 0x23893e81, 0xd396acc5,\r\n\t\t0x0f6d6ff3, 0x83f44239, 0x2e0b4482, 0xa4842004, 0x69c8f04a,\r\n\t\t0x9e1f9b5e, 0x21c66842, 0xf6e96c9a, 0x670c9c61, 0xabd388f0,\r\n\t\t0x6a51a0d2, 0xd8542f68, 0x960fa728, 0xab5133a3, 0x6eef0b6c,\r\n\t\t0x137a3be4, 0xba3bf050, 0x7efb2a98, 0xa1f1651d, 0x39af0176,\r\n\t\t0x66ca593e, 0x82430e88, 0x8cee8619, 0x456f9fb4, 0x7d84a5c3,\r\n\t\t0x3b8b5ebe, 0xe06f75d8, 0x85c12073, 0x401a449f, 0x56c16aa6,\r\n\t\t0x4ed3aa62, 0x363f7706, 0x1bfedf72, 0x429b023d, 0x37d0d724,\r\n\t\t0xd00a1248, 0xdb0fead3, 0x49f1c09b, 0x075372c9, 0x80991b7b,\r\n\t\t0x25d479d8, 0xf6e8def7, 0xe3fe501a, 0xb6794c3b, 0x976ce0bd,\r\n\t\t0x04c006ba, 0xc1a94fb6, 0x409f60c4, 0x5e5c9ec2, 0x196a2463,\r\n\t\t0x68fb6faf, 0x3e6c53b5, 0x1339b2eb, 0x3b52ec6f, 0x6dfc511f,\r\n\t\t0x9b30952c, 0xcc814544, 0xaf5ebd09, 0xbee3d004, 0xde334afd,\r\n\t\t0x660f2807, 0x192e4bb3, 0xc0cba857, 0x45c8740f, 0xd20b5f39,\r\n\t\t0xb9d3fbdb, 0x5579c0bd, 0x1a60320a, 0xd6a100c6, 0x402c7279,\r\n\t\t0x679f25fe, 0xfb1fa3cc, 0x8ea5e9f8, 0xdb3222f8, 0x3c7516df,\r\n\t\t0xfd616b15, 0x2f501ec8, 0xad0552ab, 0x323db5fa, 0xfd238760,\r\n\t\t0x53317b48, 0x3e00df82, 0x9e5c57bb, 0xca6f8ca0, 0x1a87562e,\r\n\t\t0xdf1769db, 0xd542a8f6, 0x287effc3, 0xac6732c6, 0x8c4f5573,\r\n\t\t0x695b27b0, 0xbbca58c8, 0xe1ffa35d, 0xb8f011a0, 0x10fa3d98,\r\n\t\t0xfd2183b8, 0x4afcb56c, 0x2dd1d35b, 0x9a53e479, 0xb6f84565,\r\n\t\t0xd28e49bc, 0x4bfb9790, 0xe1ddf2da, 0xa4cb7e33, 0x62fb1341,\r\n\t\t0xcee4c6e8, 0xef20cada, 0x36774c01, 0xd07e9efe, 0x2bf11fb4,\r\n\t\t0x95dbda4d, 0xae909198, 0xeaad8e71, 0x6b93d5a0, 0xd08ed1d0,\r\n\t\t0xafc725e0, 0x8e3c5b2f, 0x8e7594b7, 0x8ff6e2fb, 0xf2122b64,\r\n\t\t0x8888b812, 0x900df01c, 0x4fad5ea0, 0x688fc31c, 0xd1cff191,\r\n\t\t0xb3a8c1ad, 0x2f2f2218, 0xbe0e1777, 0xea752dfe, 0x8b021fa1,\r\n\t\t0xe5a0cc0f, 0xb56f74e8, 0x18acf3d6, 0xce89e299, 0xb4a84fe0,\r\n\t\t0xfd13e0b7, 0x7cc43b81, 0xd2ada8d9, 0x165fa266, 0x80957705,\r\n\t\t0x93cc7314, 0x211a1477, 0xe6ad2065, 0x77b5fa86, 0xc75442f5,\r\n\t\t0xfb9d35cf, 0xebcdaf0c, 0x7b3e89a0, 0xd6411bd3, 0xae1e7e49,\r\n\t\t0x00250e2d, 0x2071b35e, 0x226800bb, 0x57b8e0af, 0x2464369b,\r\n\t\t0xf009b91e, 0x5563911d, 0x59dfa6aa, 0x78c14389, 0xd95a537f,\r\n\t\t0x207d5ba2, 0x02e5b9c5, 0x83260376, 0x6295cfa9, 0x11c81968,\r\n\t\t0x4e734a41, 0xb3472dca, 0x7b14a94a, 0x1b510052, 0x9a532915,\r\n\t\t0xd60f573f, 0xbc9bc6e4, 0x2b60a476, 0x81e67400, 0x08ba6fb5,\r\n\t\t0x571be91f, 0xf296ec6b, 0x2a0dd915, 0xb6636521, 0xe7b9f9b6,\r\n\t\t0xff34052e, 0xc5855664, 0x53b02d5d, 0xa99f8fa1, 0x08ba4799,\r\n\t\t0x6e85076a, 0x4b7a70e9, 0xb5b32944, 0xdb75092e, 0xc4192623,\r\n\t\t0xad6ea6b0, 0x49a7df7d, 0x9cee60b8, 0x8fedb266, 0xecaa8c71,\r\n\t\t0x699a17ff, 0x5664526c, 0xc2b19ee1, 0x193602a5, 0x75094c29,\r\n\t\t0xa0591340, 0xe4183a3e, 0x3f54989a, 0x5b429d65, 0x6b8fe4d6,\r\n\t\t0x99f73fd6, 0xa1d29c07, 0xefe830f5, 0x4d2d38e6, 0xf0255dc1,\r\n\t\t0x4cdd2086, 0x8470eb26, 0x6382e9c6, 0x021ecc5e, 0x09686b3f,\r\n\t\t0x3ebaefc9, 0x3c971814, 0x6b6a70a1, 0x687f3584, 0x52a0e286,\r\n\t\t0xb79c5305, 0xaa500737, 0x3e07841c, 0x7fdeae5c, 0x8e7d44ec,\r\n\t\t0x5716f2b8, 0xb03ada37, 0xf0500c0d, 0xf01c1f04, 0x0200b3ff,\r\n\t\t0xae0cf51a, 0x3cb574b2, 0x25837a58, 0xdc0921bd, 0xd19113f9,\r\n\t\t0x7ca92ff6, 0x94324773, 0x22f54701, 0x3ae5e581, 0x37c2dadc,\r\n\t\t0xc8b57634, 0x9af3dda7, 0xa9446146, 0x0fd0030e, 0xecc8c73e,\r\n\t\t0xa4751e41, 0xe238cd99, 0x3bea0e2f, 0x3280bba1, 0x183eb331,\r\n\t\t0x4e548b38, 0x4f6db908, 0x6f420d03, 0xf60a04bf, 0x2cb81290,\r\n\t\t0x24977c79, 0x5679b072, 0xbcaf89af, 0xde9a771f, 0xd9930810,\r\n\t\t0xb38bae12, 0xdccf3f2e, 0x5512721f, 0x2e6b7124, 0x501adde6,\r\n\t\t0x9f84cd87, 0x7a584718, 0x7408da17, 0xbc9f9abc, 0xe94b7d8c,\r\n\t\t0xec7aec3a, 0xdb851dfa, 0x63094366, 0xc464c3d2, 0xef1c1847,\r\n\t\t0x3215d908, 0xdd433b37, 0x24c2ba16, 0x12a14d43, 0x2a65c451,\r\n\t\t0x50940002, 0x133ae4dd, 0x71dff89e, 0x10314e55, 0x81ac77d6,\r\n\t\t0x5f11199b, 0x043556f1, 0xd7a3c76b, 0x3c11183b, 0x5924a509,\r\n\t\t0xf28fe6ed, 0x97f1fbfa, 0x9ebabf2c, 0x1e153c6e, 0x86e34570,\r\n\t\t0xeae96fb1, 0x860e5e0a, 0x5a3e2ab3, 0x771fe71c, 0x4e3d06fa,\r\n\t\t0x2965dcb9, 0x99e71d0f, 0x803e89d6, 0x5266c825, 0x2e4cc978,\r\n\t\t0x9c10b36a, 0xc6150eba, 0x94e2ea78, 0xa5fc3c53, 0x1e0a2df4,\r\n\t\t0xf2f74ea7, 0x361d2b3d, 0x1939260f, 0x19c27960, 0x5223a708,\r\n\t\t0xf71312b6, 0xebadfe6e, 0xeac31f66, 0xe3bc4595, 0xa67bc883,\r\n\t\t0xb17f37d1, 0x018cff28, 0xc332ddef, 0xbe6c5aa5, 0x65582185,\r\n\t\t0x68ab9802, 0xeecea50f, 0xdb2f953b, 0x2aef7dad, 0x5b6e2f84,\r\n\t\t0x1521b628, 0x29076170, 0xecdd4775, 0x619f1510, 0x13cca830,\r\n\t\t0xeb61bd96, 0x0334fe1e, 0xaa0363cf, 0xb5735c90, 0x4c70a239,\r\n\t\t0xd59e9e0b, 0xcbaade14, 0xeecc86bc, 0x60622ca7, 0x9cab5cab,\r\n\t\t0xb2f3846e, 0x648b1eaf, 0x19bdf0ca, 0xa02369b9, 0x655abb50,\r\n\t\t0x40685a32, 0x3c2ab4b3, 0x319ee9d5, 0xc021b8f7, 0x9b540b19,\r\n\t\t0x875fa099, 0x95f7997e, 0x623d7da8, 0xf837889a, 0x97e32d77,\r\n\t\t0x11ed935f, 0x16681281, 0x0e358829, 0xc7e61fd6, 0x96dedfa1,\r\n\t\t0x7858ba99, 0x57f584a5, 0x1b227263, 0x9b83c3ff, 0x1ac24696,\r\n\t\t0xcdb30aeb, 0x532e3054, 0x8fd948e4, 0x6dbc3128, 0x58ebf2ef,\r\n\t\t0x34c6ffea, 0xfe28ed61, 0xee7c3c73, 0x5d4a14d9, 0xe864b7e3,\r\n\t\t0x42105d14, 0x203e13e0, 0x45eee2b6, 0xa3aaabea, 0xdb6c4f15,\r\n\t\t0xfacb4fd0, 0xc742f442, 0xef6abbb5, 0x654f3b1d, 0x41cd2105,\r\n\t\t0xd81e799e, 0x86854dc7, 0xe44b476a, 0x3d816250, 0xcf62a1f2,\r\n\t\t0x5b8d2646, 0xfc8883a0, 0xc1c7b6a3, 0x7f1524c3, 0x69cb7492,\r\n\t\t0x47848a0b, 0x5692b285, 0x095bbf00, 0xad19489d, 0x1462b174,\r\n\t\t0x23820e00, 0x58428d2a, 0x0c55f5ea, 0x1dadf43e, 0x233f7061,\r\n\t\t0x3372f092, 0x8d937e41, 0xd65fecf1, 0x6c223bdb, 0x7cde3759,\r\n\t\t0xcbee7460, 0x4085f2a7, 0xce77326e, 0xa6078084, 0x19f8509e,\r\n\t\t0xe8efd855, 0x61d99735, 0xa969a7aa, 0xc50c06c2, 0x5a04abfc,\r\n\t\t0x800bcadc, 0x9e447a2e, 0xc3453484, 0xfdd56705, 0x0e1e9ec9,\r\n\t\t0xdb73dbd3, 0x105588cd, 0x675fda79, 0xe3674340, 0xc5c43465,\r\n\t\t0x713e38d8, 0x3d28f89e, 0xf16dff20, 0x153e21e7, 0x8fb03d4a,\r\n\t\t0xe6e39f2b, 0xdb83adf7, 0xe93d5a68, 0x948140f7, 0xf64c261c,\r\n\t\t0x94692934, 0x411520f7, 0x7602d4f7, 0xbcf46b2e, 0xd4a20068,\r\n\t\t0xd4082471, 0x3320f46a, 0x43b7d4b7, 0x500061af, 0x1e39f62e,\r\n\t\t0x97244546, 0x14214f74, 0xbf8b8840, 0x4d95fc1d, 0x96b591af,\r\n\t\t0x70f4ddd3, 0x66a02f45, 0xbfbc09ec, 0x03bd9785, 0x7fac6dd0,\r\n\t\t0x31cb8504, 0x96eb27b3, 0x55fd3941, 0xda2547e6, 0xabca0a9a,\r\n\t\t0x28507825, 0x530429f4, 0x0a2c86da, 0xe9b66dfb, 0x68dc1462,\r\n\t\t0xd7486900, 0x680ec0a4, 0x27a18dee, 0x4f3ffea2, 0xe887ad8c,\r\n\t\t0xb58ce006, 0x7af4d6b6, 0xaace1e7c, 0xd3375fec, 0xce78a399,\r\n\t\t0x406b2a42, 0x20fe9e35, 0xd9f385b9, 0xee39d7ab, 0x3b124e8b,\r\n\t\t0x1dc9faf7, 0x4b6d1856, 0x26a36631, 0xeae397b2, 0x3a6efa74,\r\n\t\t0xdd5b4332, 0x6841e7f7, 0xca7820fb, 0xfb0af54e, 0xd8feb397,\r\n\t\t0x454056ac, 0xba489527, 0x55533a3a, 0x20838d87, 0xfe6ba9b7,\r\n\t\t0xd096954b, 0x55a867bc, 0xa1159a58, 0xcca92963, 0x99e1db33,\r\n\t\t0xa62a4a56, 0x3f3125f9, 0x5ef47e1c, 0x9029317c, 0xfdf8e802,\r\n\t\t0x04272f70, 0x80bb155c, 0x05282ce3, 0x95c11548, 0xe4c66d22,\r\n\t\t0x48c1133f, 0xc70f86dc, 0x07f9c9ee, 0x41041f0f, 0x404779a4,\r\n\t\t0x5d886e17, 0x325f51eb, 0xd59bc0d1, 0xf2bcc18f, 0x41113564,\r\n\t\t0x257b7834, 0x602a9c60, 0xdff8e8a3, 0x1f636c1b, 0x0e12b4c2,\r\n\t\t0x02e1329e, 0xaf664fd1, 0xcad18115, 0x6b2395e0, 0x333e92e1,\r\n\t\t0x3b240b62, 0xeebeb922, 0x85b2a20e, 0xe6ba0d99, 0xde720c8c,\r\n\t\t0x2da2f728, 0xd0127845, 0x95b794fd, 0x647d0862, 0xe7ccf5f0,\r\n\t\t0x5449a36f, 0x877d48fa, 0xc39dfd27, 0xf33e8d1e, 0x0a476341,\r\n\t\t0x992eff74, 0x3a6f6eab, 0xf4f8fd37, 0xa812dc60, 0xa1ebddf8,\r\n\t\t0x991be14c, 0xdb6e6b0d, 0xc67b5510, 0x6d672c37, 0x2765d43b,\r\n\t\t0xdcd0e804, 0xf1290dc7, 0xcc00ffa3, 0xb5390f92, 0x690fed0b,\r\n\t\t0x667b9ffb, 0xcedb7d9c, 0xa091cf0b, 0xd9155ea3, 0xbb132f88,\r\n\t\t0x515bad24, 0x7b9479bf, 0x763bd6eb, 0x37392eb3, 0xcc115979,\r\n\t\t0x8026e297, 0xf42e312d, 0x6842ada7, 0xc66a2b3b, 0x12754ccc,\r\n\t\t0x782ef11c, 0x6a124237, 0xb79251e7, 0x06a1bbe6, 0x4bfb6350,\r\n\t\t0x1a6b1018, 0x11caedfa, 0x3d25bdd8, 0xe2e1c3c9, 0x44421659,\r\n\t\t0x0a121386, 0xd90cec6e, 0xd5abea2a, 0x64af674e, 0xda86a85f,\r\n\t\t0xbebfe988, 0x64e4c3fe, 0x9dbc8057, 0xf0f7c086, 0x60787bf8,\r\n\t\t0x6003604d, 0xd1fd8346, 0xf6381fb0, 0x7745ae04, 0xd736fccc,\r\n\t\t0x83426b33, 0xf01eab71, 0xb0804187, 0x3c005e5f, 0x77a057be,\r\n\t\t0xbde8ae24, 0x55464299, 0xbf582e61, 0x4e58f48f, 0xf2ddfda2,\r\n\t\t0xf474ef38, 0x8789bdc2, 0x5366f9c3, 0xc8b38e74, 0xb475f255,\r\n\t\t0x46fcd9b9, 0x7aeb2661, 0x8b1ddf84, 0x846a0e79, 0x915f95e2,\r\n\t\t0x466e598e, 0x20b45770, 0x8cd55591, 0xc902de4c, 0xb90bace1,\r\n\t\t0xbb8205d0, 0x11a86248, 0x7574a99e, 0xb77f19b6, 0xe0a9dc09,\r\n\t\t0x662d09a1, 0xc4324633, 0xe85a1f02, 0x09f0be8c, 0x4a99a025,\r\n\t\t0x1d6efe10, 0x1ab93d1d, 0x0ba5a4df, 0xa186f20f, 0x2868f169,\r\n\t\t0xdcb7da83, 0x573906fe, 0xa1e2ce9b, 0x4fcd7f52, 0x50115e01,\r\n\t\t0xa70683fa, 0xa002b5c4, 0x0de6d027, 0x9af88c27, 0x773f8641,\r\n\t\t0xc3604c06, 0x61a806b5, 0xf0177a28, 0xc0f586e0, 0x006058aa,\r\n\t\t0x30dc7d62, 0x11e69ed7, 0x2338ea63, 0x53c2dd94, 0xc2c21634,\r\n\t\t0xbbcbee56, 0x90bcb6de, 0xebfc7da1, 0xce591d76, 0x6f05e409,\r\n\t\t0x4b7c0188, 0x39720a3d, 0x7c927c24, 0x86e3725f, 0x724d9db9,\r\n\t\t0x1ac15bb4, 0xd39eb8fc, 0xed545578, 0x08fca5b5, 0xd83d7cd3,\r\n\t\t0x4dad0fc4, 0x1e50ef5e, 0xb161e6f8, 0xa28514d9, 0x6c51133c,\r\n\t\t0x6fd5c7e7, 0x56e14ec4, 0x362abfce, 0xddc6c837, 0xd79a3234,\r\n\t\t0x92638212, 0x670efa8e, 0x406000e0, 0x3a39ce37, 0xd3faf5cf,\r\n\t\t0xabc27737, 0x5ac52d1b, 0x5cb0679e, 0x4fa33742, 0xd3822740,\r\n\t\t0x99bc9bbe, 0xd5118e9d, 0xbf0f7315, 0xd62d1c7e, 0xc700c47b,\r\n\t\t0xb78c1b6b, 0x21a19045, 0xb26eb1be, 0x6a366eb4, 0x5748ab2f,\r\n\t\t0xbc946e79, 0xc6a376d2, 0x6549c2c8, 0x530ff8ee, 0x468dde7d,\r\n\t\t0xd5730a1d, 0x4cd04dc6, 0x2939bbdb, 0xa9ba4650, 0xac9526e8,\r\n\t\t0xbe5ee304, 0xa1fad5f0, 0x6a2d519a, 0x63ef8ce2, 0x9a86ee22,\r\n\t\t0xc089c2b8, 0x43242ef6, 0xa51e03aa, 0x9cf2d0a4, 0x83c061ba,\r\n\t\t0x9be96a4d, 0x8fe51550, 0xba645bd6, 0x2826a2f9, 0xa73a3ae1,\r\n\t\t0x4ba99586, 0xef5562e9, 0xc72fefd3, 0xf752f7da, 0x3f046f69,\r\n\t\t0x77fa0a59, 0x80e4a915, 0x87b08601, 0x9b09e6ad, 0x3b3ee593,\r\n\t\t0xe990fd5a, 0x9e34d797, 0x2cf0b7d9, 0x022b8b51, 0x96d5ac3a,\r\n\t\t0x017da67d, 0xd1cf3ed6, 0x7c7d2d28, 0x1f9f25cf, 0xadf2b89b,\r\n\t\t0x5ad6b472, 0x5a88f54c, 0xe029ac71, 0xe019a5e6, 0x47b0acfd,\r\n\t\t0xed93fa9b, 0xe8d3c48d, 0x283b57cc, 0xf8d56629, 0x79132e28,\r\n\t\t0x785f0191, 0xed756055, 0xf7960e44, 0xe3d35e8c, 0x15056dd4,\r\n\t\t0x88f46dba, 0x03a16125, 0x0564f0bd, 0xc3eb9e15, 0x3c9057a2,\r\n\t\t0x97271aec, 0xa93a072a, 0x1b3f6d9b, 0x1e6321f5, 0xf59c66fb,\r\n\t\t0x26dcf319, 0x7533d928, 0xb155fdf5, 0x03563482, 0x8aba3cbb,\r\n\t\t0x28517711, 0xc20ad9f8, 0xabcc5167, 0xccad925f, 0x4de81751,\r\n\t\t0x3830dc8e, 0x379d5862, 0x9320f991, 0xea7a90c2, 0xfb3e7bce,\r\n\t\t0x5121ce64, 0x774fbe32, 0xa8b6e37e, 0xc3293d46, 0x48de5369,\r\n\t\t0x6413e680, 0xa2ae0810, 0xdd6db224, 0x69852dfd, 0x09072166,\r\n\t\t0xb39a460a, 0x6445c0dd, 0x586cdecf, 0x1c20c8ae, 0x5bbef7dd,\r\n\t\t0x1b588d40, 0xccd2017f, 0x6bb4e3bb, 0xdda26a7e, 0x3a59ff45,\r\n\t\t0x3e350a44, 0xbcb4cdd5, 0x72eacea8, 0xfa6484bb, 0x8d6612ae,\r\n\t\t0xbf3c6f47, 0xd29be463, 0x542f5d9e, 0xaec2771b, 0xf64e6370,\r\n\t\t0x740e0d8d, 0xe75b1357, 0xf8721671, 0xaf537d5d, 0x4040cb08,\r\n\t\t0x4eb4e2cc, 0x34d2466a, 0x0115af84, 0xe1b00428, 0x95983a1d,\r\n\t\t0x06b89fb4, 0xce6ea048, 0x6f3f3b82, 0x3520ab82, 0x011a1d4b,\r\n\t\t0x277227f8, 0x611560b1, 0xe7933fdc, 0xbb3a792b, 0x344525bd,\r\n\t\t0xa08839e1, 0x51ce794b, 0x2f32c9b7, 0xa01fbac9, 0xe01cc87e,\r\n\t\t0xbcc7d1f6, 0xcf0111c3, 0xa1e8aac7, 0x1a908749, 0xd44fbd9a,\r\n\t\t0xd0dadecb, 0xd50ada38, 0x0339c32a, 0xc6913667, 0x8df9317c,\r\n\t\t0xe0b12b4f, 0xf79e59b7, 0x43f5bb3a, 0xf2d519ff, 0x27d9459c,\r\n\t\t0xbf97222c, 0x15e6fc2a, 0x0f91fc71, 0x9b941525, 0xfae59361,\r\n\t\t0xceb69ceb, 0xc2a86459, 0x12baa8d1, 0xb6c1075e, 0xe3056a0c,\r\n\t\t0x10d25065, 0xcb03a442, 0xe0ec6e0e, 0x1698db3b, 0x4c98a0be,\r\n\t\t0x3278e964, 0x9f1f9532, 0xe0d392df, 0xd3a0342b, 0x8971f21e,\r\n\t\t0x1b0a7441, 0x4ba3348c, 0xc5be7120, 0xc37632d8, 0xdf359f8d,\r\n\t\t0x9b992f2e, 0xe60b6f47, 0x0fe3f11d, 0xe54cda54, 0x1edad891,\r\n\t\t0xce6279cf, 0xcd3e7e6f, 0x1618b166, 0xfd2c1d05, 0x848fd2c5,\r\n\t\t0xf6fb2299, 0xf523f357, 0xa6327623, 0x93a83531, 0x56cccd02,\r\n\t\t0xacf08162, 0x5a75ebb5, 0x6e163697, 0x88d273cc, 0xde966292,\r\n\t\t0x81b949d0, 0x4c50901b, 0x71c65614, 0xe6c6c7bd, 0x327a140a,\r\n\t\t0x45e1d006, 0xc3f27b9a, 0xc9aa53fd, 0x62a80f00, 0xbb25bfe2,\r\n\t\t0x35bdd2f6, 0x71126905, 0xb2040222, 0xb6cbcf7c, 0xcd769c2b,\r\n\t\t0x53113ec0, 0x1640e3d3, 0x38abbd60, 0x2547adf0, 0xba38209c,\r\n\t\t0xf746ce76, 0x77afa1c5, 0x20756060, 0x85cbfe4e, 0x8ae88dd8,\r\n\t\t0x7aaaf9b0, 0x4cf9aa7e, 0x1948c25c, 0x02fb8a8c, 0x01c36ae4,\r\n\t\t0xd6ebe1f9, 0x90d4f869, 0xa65cdea0, 0x3f09252d, 0xc208e69f,\r\n\t\t0xb74e6132, 0xce77e25b, 0x578fdfe3, 0x3ac372e6];\r\nvar bf_crypt_ciphertext = [0x4f727068, 0x65616e42, 0x65686f6c, 0x64657253,\r\n\t\t0x63727944, 0x6f756274];\r\nvar base64_code = ['.', '/', 'A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I',\r\n\t\t'J', 'K', 'L', 'M', 'N', 'O', 'P', 'Q', 'R', 'S', 'T', 'U', 'V',\r\n\t\t'W', 'X', 'Y', 'Z', 'a', 'b', 'c', 'd', 'e', 'f', 'g', 'h', 'i',\r\n\t\t'j', 'k', 'l', 'm', 'n', 'o', 'p', 'q', 'r', 's', 't', 'u', 'v',\r\n\t\t'w', 'x', 'y', 'z', '0', '1', '2', '3', '4', '5', '6', '7', '8',\r\n\t\t'9'];\r\nvar index_64 = [-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,\r\n\t\t-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1,\r\n\t\t-1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, -1, 0, 1,\r\n\t\t54, 55, 56, 57, 58, 59, 60, 61, 62, 63, -1, -1, -1, -1, -1, -1, -1,\r\n\t\t2, 3, 4, 5, 6, 7, 8, 9, 10, 11, 12, 13, 14, 15, 16, 17, 18, 19, 20,\r\n\t\t21, 22, 23, 24, 25, 26, 27, -1, -1, -1, -1, -1, -1, 28, 29, 30, 31,\r\n\t\t32, 33, 34, 35, 36, 37, 38, 39, 40, 41, 42, 43, 44, 45, 46, 47, 48,\r\n\t\t49, 50, 51, 52, 53, -1, -1, -1, -1, -1];\r\n\r\nfunction getByte(c) {\r\n\tvar ret = 0;\r\n\ttry {\r\n\t\tvar b = c.charCodeAt(0);\r\n\t} catch (err) {\r\n\t\tb = c;\r\n\t}\r\n\tif (b > 127) {\r\n\t\treturn -128 + (b % 128);\r\n\t} else {\r\n\t\treturn b;\r\n\t}\r\n};\r\n\r\nfunction encode_base64(d, len) {\r\n\tvar off = 0;\r\n\tvar rs = [];\r\n\tvar c1;\r\n\tvar c2;\r\n\tif (len <= 0 || len > d.length)\r\n\t\tthrow \"Invalid len\";\r\n\twhile (off < len) {\r\n\t\tc1 = d[off++] & 0xff;\r\n\t\trs.push(base64_code[(c1 >> 2) & 0x3f]);\r\n\t\tc1 = (c1 & 0x03) << 4;\r\n\t\tif (off >= len) {\r\n\t\t\trs.push(base64_code[c1 & 0x3f]);\r\n\t\t\tbreak;\r\n\t\t}\r\n\t\tc2 = d[off++] & 0xff;\r\n\t\tc1 |= (c2 >> 4) & 0x0f;\r\n\t\trs.push(base64_code[c1 & 0x3f]);\r\n\t\tc1 = (c2 & 0x0f) << 2;\r\n\t\tif (off >= len) {\r\n\t\t\trs.push(base64_code[c1 & 0x3f]);\r\n\t\t\tbreak;\r\n\t\t}\r\n\t\tc2 = d[off++] & 0xff;\r\n\t\tc1 |= (c2 >> 6) & 0x03;\r\n\t\trs.push(base64_code[c1 & 0x3f]);\r\n\t\trs.push(base64_code[c2 & 0x3f]);\r\n\t}\r\n\treturn rs.join('');\r\n};\r\n\r\nfunction char64(x) {\r\n\tvar code = x.charCodeAt(0);\r\n\tif (code < 0 || code > index_64.length) {\r\n\t\treturn -1;\r\n\t}\r\n\treturn index_64[code];\r\n};\r\n\r\nfunction decode_base64(s, maxolen) {\r\n\tvar off = 0;\r\n\tvar slen = s.length;\r\n\tvar olen = 0;\r\n\tvar rs = [];\r\n\tvar c1, c2, c3, c4, o;\r\n\tif (maxolen <= 0) throw \"Invalid maxolen\";\r\n\twhile (off < slen - 1 && olen < maxolen) {\r\n\t\tc1 = char64(s.charAt(off++));\r\n\t\tc2 = char64(s.charAt(off++));\r\n\t\tif (c1 == -1 || c2 == -1) {\r\n\t\t\tbreak;\r\n\t\t}\r\n\t\to = getByte(c1 << 2);\r\n\t\to |= (c2 & 0x30) >> 4;\r\n\t\trs.push(String.fromCharCode(o));\r\n\t\tif (++olen >= maxolen || off >= slen) {\r\n\t\t\tbreak;\r\n\t\t}\r\n\t\tc3 = char64(s.charAt(off++));\r\n\t\tif (c3 == -1) {\r\n\t\t\tbreak;\r\n\t\t}\r\n\t\to = getByte((c2 & 0x0f) << 4);\r\n\t\to |= (c3 & 0x3c) >> 2;\r\n\t\trs.push(String.fromCharCode(o));\r\n\t\tif (++olen >= maxolen || off >= slen) {\r\n\t\t\tbreak;\r\n\t\t}\r\n\t\tc4 = char64(s.charAt(off++));\r\n\t\to = getByte((c3 & 0x03) << 6);\r\n\t\to |= c4;\r\n\t\trs.push(String.fromCharCode(o));\r\n\t\t++olen;\r\n\t}\r\n\tvar ret = [];\r\n\tfor (off = 0; off < olen; off++) {\r\n\t\tret.push(getByte(rs[off]));\r\n\t}\r\n\treturn ret;\r\n};\r\n\r\nfunction encipher(lr, off, P, S) {\r\n\tvar i;\r\n\tvar n;\r\n\tvar l = lr[off];\r\n\tvar r = lr[off + 1];\r\n\r\n\tl ^= P[0];\r\n\tfor (i = 0; i <= BLOWFISH_NUM_ROUNDS - 2;) {\r\n\t\t// Feistel substitution on left word\r\n\t\tn = S[(l >> 24) & 0xff];\r\n\t\tn += S[0x100 | ((l >> 16) & 0xff)];\r\n\t\tn ^= S[0x200 | ((l >> 8) & 0xff)];\r\n\t\tn += S[0x300 | (l & 0xff)];\r\n\t\tr ^= n ^ P[++i];\r\n\r\n\t\t// Feistel substitution on right word\r\n\t\tn = S[(r >> 24) & 0xff];\r\n\t\tn += S[0x100 | ((r >> 16) & 0xff)];\r\n\t\tn ^= S[0x200 | ((r >> 8) & 0xff)];\r\n\t\tn += S[0x300 | (r & 0xff)];\r\n\t\tl ^= n ^ P[++i];\r\n\t}\r\n\tlr[off] = r ^ P[BLOWFISH_NUM_ROUNDS + 1];\r\n\tlr[off + 1] = l;\r\n\treturn lr;\r\n};\r\n\r\nfunction streamtoword(data, offp) {\r\n\tvar i;\r\n\tvar word = 0;\r\n\tfor (i = 0; i < 4; i++) {\r\n\t\tword = (word << 8) | (data[offp] & 0xff);\r\n\t\toffp = (offp + 1) % data.length;\r\n\t}\r\n\treturn {key:word, offp:offp};\r\n};\r\n\r\nfunction key(key, P, S) {\r\n\tvar i;\r\n\tvar offp = 0;\r\n\tvar lr = new Array(0x00000000, 0x00000000);\r\n\tvar plen = P.length;\r\n\tvar slen = S.length;\r\n\r\n\tfor (i = 0; i < plen; i++) {\r\n\t\tvar sw = streamtoword(key, offp);\r\n\t\toffp = sw.offp;\r\n\t\tP[i] = P[i] ^ sw.key;\r\n\t}\r\n\tfor (i = 0; i < plen; i += 2) {\r\n\t\tlr = encipher(lr, 0, P, S);\r\n\t\tP[i] = lr[0];\r\n\t\tP[i + 1] = lr[1];\r\n\t}\r\n\r\n\tfor (i = 0; i < slen; i += 2) {\r\n\t\tlr = encipher(lr, 0, P, S);\r\n\t\tS[i] = lr[0];\r\n\t\tS[i + 1] = lr[1];\r\n\t}\r\n};\r\n\r\nfunction ekskey(data, key, P, S) {\r\n\tvar i;\r\n\tvar offp = 0;\r\n\tvar lr = new Array(0x00000000, 0x00000000);\r\n\tvar plen = P.length;\r\n\tvar slen = S.length;\r\n\tvar sw;\r\n\r\n\tfor (i = 0; i < plen; i++) {\r\n\t\tsw = streamtoword(key, offp);\r\n\t\toffp = sw.offp;\r\n\t\tP[i] = P[i] ^ sw.key;\r\n\t}\r\n\toffp = 0;\r\n\tfor (i = 0; i < plen; i += 2) {\r\n\t\tsw = streamtoword(data, offp);\r\n\t\toffp = sw.offp;\r\n\t\tlr[0] ^= sw.key;\r\n\r\n\t\tsw = streamtoword(data, offp);\r\n\t\toffp = sw.offp;\r\n\t\tlr[1] ^= sw.key;\r\n\r\n\t\tlr = encipher(lr, 0, P, S);\r\n\t\tP[i] = lr[0];\r\n\t\tP[i + 1] = lr[1];\r\n\t}\r\n\tfor (i = 0; i < slen; i += 2) {\r\n\t\tsw = streamtoword(data, offp);\r\n\t\toffp = sw.offp;\r\n\t\tlr[0] ^= sw.key;\r\n\r\n\t\tsw = streamtoword(data, offp);\r\n\t\toffp = sw.offp;\r\n\t\tlr[1] ^= sw.key;\r\n\r\n\t\tlr = encipher(lr, 0, P, S);\r\n\t\tS[i] = lr[0];\r\n\t\tS[i + 1] = lr[1];\r\n\t}\r\n};\r\n\r\nfunction crypt_raw(password, salt, log_rounds, progress) {\r\n\tvar rounds;\r\n\tvar j;\r\n\tvar cdata = bf_crypt_ciphertext.slice();\r\n\tvar clen = cdata.length;\r\n\tvar one_percent;\r\n\r\n\tif (log_rounds < 4 || log_rounds > 31)\r\n\t\tthrow \"Bad number of rounds\";\r\n\tif (salt.length != BCRYPT_SALT_LEN)\r\n\t\tthrow \"Bad salt length\";\r\n\r\n\trounds = 1 << log_rounds;\r\n\tone_percent = Math.floor(rounds / 100) + 1;\r\n\r\n\tvar P = P_orig.slice();\r\n\tvar S = S_orig.slice();\r\n\r\n\tekskey(salt, password, P, S);\r\n\r\n\tvar i = 0;\r\n\r\n\twhile(true) {\r\n\t\tif(i < rounds){\r\n\t\t\tvar start = new Date();\r\n\t\t\tfor (; i < rounds;) {\r\n\t\t\t\ti = i + 1;\r\n\t\t\t\tkey(password, P, S);\r\n\t\t\t\tkey(salt, P, S);\r\n\t\t                if(i % one_percent == 0){\r\n\t\t\t        \tprogress();\r\n                \t\t}\r\n\t\t                if((new Date() - start) > MAX_EXECUTION_TIME){\r\n                    \t\t\tbreak;\r\n\t\t                }\r\n            \t\t}\r\n        \t} else {\r\n \t        \tfor (i = 0; i < 64; i++) {\r\n                \t\tfor (j = 0; j < (clen >> 1); j++) {\r\n                    \t\t\tlr = encipher(cdata, j << 1, P, S);\r\n                \t\t}\r\n            \t\t}\r\n\t\t\tvar ret = [];\r\n\t\t        for (i = 0; i < clen; i++) {\r\n                \t\tret.push(getByte((cdata[i] >> 24) & 0xff));\r\n                \t\tret.push(getByte((cdata[i] >> 16) & 0xff));\r\n                \t\tret.push(getByte((cdata[i] >> 8) & 0xff));\r\n                \t\tret.push(getByte(cdata[i] & 0xff));\r\n            \t\t}\r\n            \t\treturn(ret);\r\n        \t}\r\n\t}\r\n};\r\n\r\nfunction hashpw(password, salt, progress) {\r\n\tvar real_salt;\r\n\tvar passwordb = [];\r\n\tvar saltb = [];\r\n\tvar hashed = [];\r\n\tvar minor = String.fromCharCode(0);\r\n\tvar rounds = 0;\r\n\tvar off = 0;\r\n\r\n\tif (!progress){\r\n\t        var progress = function() {};\r\n\t}\r\n\r\n\tif (salt.charAt(0) != '$' || salt.charAt(1) != '2')\r\n\t\tthrow \"Invalid salt version\";\r\n\tif (salt.charAt(2) == '$')\r\n\t\toff = 3;\r\n\telse {\r\n\t\tminor = salt.charAt(2);\r\n\t\tif (minor != 'a' || salt.charAt(3) != '$')\r\n\t\t\tthrow \"Invalid salt revision\";\r\n\t\toff = 4;\r\n\t}\r\n\r\n\t// Extract number of rounds\r\n\tif (salt.charAt(off + 2) > '$')\r\n\t\tthrow \"Missing salt rounds\";\r\n\tvar r1 = parseInt(salt.substring(off, off + 1)) * 10;\r\n\tvar r2 = parseInt(salt.substring(off + 1, off + 2));\r\n\trounds = r1 + r2;\r\n\treal_salt = salt.substring(off + 3, off + 25);\r\n\tpassword = password + (minor >= 'a' ? \"\\000\" : \"\");\r\n\r\n\tvar buf = new Buffer(password);\r\n\tfor (var r = 0; r < buf.length; r++) {\r\n\t\tpasswordb.push(buf[r]);\r\n\t}\r\n\tsaltb = decode_base64(real_salt, BCRYPT_SALT_LEN);\r\n\tvar hashed = crypt_raw(passwordb, saltb, rounds, progress);\r\n\r\n\tvar rs = [];\r\n\trs.push(\"$2\");\r\n\tif (minor >= 'a')\r\n\t\trs.push(minor);\r\n\trs.push(\"$\");\r\n\tif (rounds < 10)\r\n\t\trs.push(\"0\");\r\n\trs.push(rounds.toString());\r\n\trs.push(\"$\");\r\n\trs.push(encode_base64(saltb, saltb.length));\r\n\trs.push(encode_base64(hashed, bf_crypt_ciphertext.length * 4 - 1));\r\n\r\n\treturn(rs.join(''));\r\n};\r\n\r\nfunction gensalt(rounds) {\r\n\tvar iteration_count = rounds;\r\n\tif (iteration_count < 4 || iteration_count > 31) {\r\n\t\titeration_count = GENSALT_DEFAULT_LOG2_ROUNDS;\r\n\t}\r\n\tvar output = [];\r\n\toutput.push(\"$2a$\");\r\n\tif (iteration_count < 10)\r\n\t\toutput.push(\"0\");\r\n\toutput.push(iteration_count.toString());\r\n\toutput.push('$');\r\n\r\n\tvar rand_buf;\r\n\ttry {\r\n\t\trand_buf = crypto.randomBytes(BCRYPT_SALT_LEN);\r\n\t} catch (ex) {\r\n\t\tthrow ex;\r\n\t}\r\n\r\n\toutput.push(encode_base64(rand_buf, BCRYPT_SALT_LEN));\r\n\treturn output.join('');\r\n};\r\n\r\nfunction genSaltSync(rounds) {\r\n\t/*\r\n\t\trounds - [OPTIONAL] - the number of rounds to process the data for. (default - 10)\r\n\t\tseed_length - [OPTIONAL] - RAND_bytes wants a length. to make that a bit flexible, you can specify a seed_length. (default - 20)\r\n\t*/\r\n\tif(!rounds) {\r\n\t\trounds = GENSALT_DEFAULT_LOG2_ROUNDS;\r\n\t}\r\n\treturn gensalt(rounds);\r\n}\r\n\r\nfunction genSalt(rounds, callback) {\r\n\t/*\r\n\t\trounds - [OPTIONAL] - the number of rounds to process the data for. (default - 10)\r\n\t\tseed_length - [OPTIONAL] - RAND_bytes wants a length. to make that a bit flexible, you can specify a seed_length. (default - 20)\r\n\t\tcallback - [REQUIRED] - a callback to be fired once the salt has been generated. uses eio making it asynchronous.\r\n\t\t\terror - First parameter to the callback detailing any errors.\r\n\t\t\tsalt - Second parameter to the callback providing the generated salt.\r\n\t*/\r\n\tif(!callback) {\r\n\t\tthrow \"No callback function was given.\"\r\n\t}\r\n\tprocess.nextTick(function() {\r\n\t\tvar result = null;\r\n\t\tvar error = null;\r\n\t\ttry {\r\n\t\t\tresult = genSaltSync(rounds)\r\n\t\t} catch(err) {\r\n\t\t\terror = err;\r\n\t\t}\r\n\t\tcallback(error, result);\r\n\t});\r\n}\r\n\r\nfunction hashSync(data, salt, progress) {\r\n\t/*\r\n\t\tdata - [REQUIRED] - the data to be encrypted.\r\n\t\tsalt - [REQUIRED] - the salt to be used in encryption.\r\n\t*/\r\n\tif(!salt) {\r\n\t\tsalt = genSaltSync();\r\n\t}\r\n\treturn hashpw(data, salt, progress);\r\n}\r\n\r\nfunction hash(data, salt, progress, callback) {\r\n\t/*\r\n\t\tdata - [REQUIRED] - the data to be encrypted.\r\n\t\tsalt - [REQUIRED] - the salt to be used to hash the password. if specified as a number then a salt will be generated and used (see examples).\r\n\t\tprogress - a callback to be called during the hash calculation to signify progress\r\n\t\tcallback - [REQUIRED] - a callback to be fired once the data has been encrypted. uses eio making it asynchronous.\r\n\t\t\terror - First parameter to the callback detailing any errors.\r\n\t\t\tencrypted - Second parameter to the callback providing the encrypted form.\r\n\t*/\r\n\tif(!callback) {\r\n\t\tthrow \"No callback function was given.\"\r\n\t}\r\n\tprocess.nextTick(function() {\r\n\t\tvar result = null;\r\n\t\tvar error = null;\r\n\t\ttry {\r\n\t\t\tresult = hashSync(data, salt, progress)\r\n\t\t} catch(err) {\r\n\t\t\terror = err;\r\n\t\t}\r\n\t\tcallback(error, result);\r\n\t});\r\n}\r\n\r\nfunction compareSync(data, encrypted) {\r\n\t/*\r\n\t\tdata - [REQUIRED] - data to compare.\r\n\t\tencrypted - [REQUIRED] - data to be compared to.\r\n\t*/\r\n\r\n\tif(typeof data != \"string\" ||  typeof encrypted != \"string\") {\r\n\t\tthrow \"Incorrect arguments\";\r\n\t}\r\n\r\n\tvar encrypted_length = encrypted.length;\r\n\r\n\tif(encrypted_length != 60) {\r\n\t\tthrow \"Not a valid BCrypt hash.\";\r\n\t}\r\n\r\n\tvar same = true;\r\n\tvar hash_data = hashSync(data, encrypted.substr(0, encrypted_length-31));\r\n\tvar hash_data_length = hash_data.length;\r\n\r\n\tsame = hash_data_length == encrypted_length;\r\n\r\n\tvar max_length = (hash_data_length < encrypted_length) ? hash_data_length : encrypted_length;\r\n\r\n\t// to prevent timing attacks, should check entire string\r\n\t// don't exit after found to be false\r\n\tfor (var i = 0; i < max_length; ++i) {\r\n\t\tif (hash_data_length >= i && encrypted_length >= i && hash_data[i] != encrypted[i]) {\r\n\t\t\tsame = false;\r\n\t\t}\r\n\t}\r\n\r\n\treturn same;\r\n}\r\n\r\nfunction compare(data, encrypted, callback) {\r\n\t/*\r\n\t\tdata - [REQUIRED] - data to compare.\r\n\t\tencrypted - [REQUIRED] - data to be compared to.\r\n\t\tcallback - [REQUIRED] - a callback to be fired once the data has been compared. uses eio making it asynchronous.\r\n\t\t\terror - First parameter to the callback detailing any errors.\r\n\t\t\tsame - Second parameter to the callback providing whether the data and encrypted forms match [true | false].\r\n\t*/\r\n\tif(!callback) {\r\n\t\tthrow \"No callback function was given.\"\r\n\t}\r\n\tprocess.nextTick(function() {\r\n\t\tvar result = null;\r\n\t\tvar error = null;\r\n\t\ttry {\r\n\t\t\tresult = compareSync(data, encrypted)\r\n\t\t} catch(err) {\r\n\t\t\terror = err;\r\n\t\t}\r\n\t\tcallback(error, result);\r\n\t});\r\n}\r\n\r\nfunction getRounds(encrypted) {\r\n\t//encrypted - [REQUIRED] - hash from which the number of rounds used should be extracted.\r\n\tif(typeof encrypted != \"string\") {\r\n\t\tthrow \"Incorrect arguments\";\r\n\t}\r\n\treturn Number(encrypted.split(\"$\")[2]);\r\n}\r\n\r\nexports.genSaltSync = genSaltSync;\r\nexports.genSalt = genSalt;\r\nexports.hashSync = hashSync;\r\nexports.hash = hash;\r\nexports.compareSync = compareSync;\r\nexports.compare = compare;\r\nexports.getRounds = getRounds;\r\n\n\n//# sourceURL=webpack:///./node_modules/bcrypt-nodejs/bCrypt.js?");

/***/ }),

/***/ "./src/config/constants.js":
/*!*********************************!*\
  !*** ./src/config/constants.js ***!
  \*********************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";
eval("\n\nObject.defineProperty(exports, \"__esModule\", {\n  value: true\n});\nconst devConfig = {\n  MONGO_URL: 'mongodb://localhost/faceof-dev'\n};\nconst testConfig = {\n  MONGO_URL: 'mongodb://localhost/faceof-test'\n};\nconst prodConfig = {\n  MONGO_URL: 'mongodb://localhost/faceof-prod'\n};\nconst defaultConfig = {\n  PORT: process.env.PORT || 3000\n};\n\nfunction envConfig(env) {\n  switch (env) {\n    case 'development':\n      return devConfig;\n\n    case 'test':\n      return testConfig;\n\n    default:\n      return prodConfig;\n  }\n}\n\nexports.default = Object.assign({}, defaultConfig, envConfig(\"development\"));\n\n//# sourceURL=webpack:///./src/config/constants.js?");

/***/ }),

/***/ "./src/config/database.js":
/*!********************************!*\
  !*** ./src/config/database.js ***!
  \********************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";
eval("\n\nvar _mongoose = __webpack_require__(/*! mongoose */ \"mongoose\");\n\nvar _mongoose2 = _interopRequireDefault(_mongoose);\n\nvar _constants = __webpack_require__(/*! ./constants */ \"./src/config/constants.js\");\n\nvar _constants2 = _interopRequireDefault(_constants);\n\nfunction _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }\n\n_mongoose2.default.Promise = global.Promise;\nconsole.log(_constants2.default);\n\ntry {\n  _mongoose2.default.connect(_constants2.default.MONGO_URL);\n} catch (err) {\n  _mongoose2.default.createConnection(_constants2.default.MONGO_URL);\n}\n\n_mongoose2.default.connection.once('open', () => console.log('MongoDB Running')).on('error', e => {\n  throw e;\n});\n\n//# sourceURL=webpack:///./src/config/database.js?");

/***/ }),

/***/ "./src/config/middlewares.js":
/*!***********************************!*\
  !*** ./src/config/middlewares.js ***!
  \***********************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";
eval("\n\nObject.defineProperty(exports, \"__esModule\", {\n  value: true\n});\n\nvar _morgan = __webpack_require__(/*! morgan */ \"morgan\");\n\nvar _morgan2 = _interopRequireDefault(_morgan);\n\nvar _bodyParser = __webpack_require__(/*! body-parser */ \"body-parser\");\n\nvar _bodyParser2 = _interopRequireDefault(_bodyParser);\n\nvar _compression = __webpack_require__(/*! compression */ \"compression\");\n\nvar _compression2 = _interopRequireDefault(_compression);\n\nvar _helmet = __webpack_require__(/*! helmet */ \"helmet\");\n\nvar _helmet2 = _interopRequireDefault(_helmet);\n\nfunction _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }\n\nconst isDev = \"development\" === 'development';\nconst isProd = \"development\" === 'production';\n\nexports.default = app => {\n  if (isProd) {\n    app.use((0, _compression2.default)());\n    app.use((0, _helmet2.default)());\n  }\n\n  app.use(_bodyParser2.default.json());\n  app.use(_bodyParser2.default.urlencoded({\n    extended: true\n  }));\n\n  if (isDev) {\n    app.use((0, _morgan2.default)('dev'));\n  }\n};\n\n//# sourceURL=webpack:///./src/config/middlewares.js?");

/***/ }),

/***/ "./src/index.js":
/*!**********************!*\
  !*** ./src/index.js ***!
  \**********************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";
eval("\n\nvar _express = __webpack_require__(/*! express */ \"express\");\n\nvar _express2 = _interopRequireDefault(_express);\n\nvar _constants = __webpack_require__(/*! ./config/constants */ \"./src/config/constants.js\");\n\nvar _constants2 = _interopRequireDefault(_constants);\n\n__webpack_require__(/*! ./config/database */ \"./src/config/database.js\");\n\nvar _middlewares = __webpack_require__(/*! ./config/middlewares */ \"./src/config/middlewares.js\");\n\nvar _middlewares2 = _interopRequireDefault(_middlewares);\n\nvar _modules = __webpack_require__(/*! ./modules */ \"./src/modules/index.js\");\n\nvar _modules2 = _interopRequireDefault(_modules);\n\nfunction _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }\n\nconst app = (0, _express2.default)();\n(0, _middlewares2.default)(app);\n(0, _modules2.default)(app);\napp.listen(_constants2.default.PORT, err => {\n  if (err) {\n    console.log(err);\n  } else {\n    console.log(`\n      Server running on port: ${_constants2.default.PORT}\n      ------\n      Running on ${\"development\"}\n    `);\n  }\n});\n\n//# sourceURL=webpack:///./src/index.js?");

/***/ }),

/***/ "./src/modules/index.js":
/*!******************************!*\
  !*** ./src/modules/index.js ***!
  \******************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";
eval("\n\nObject.defineProperty(exports, \"__esModule\", {\n  value: true\n});\n\nvar _user = __webpack_require__(/*! ./users/user.routes */ \"./src/modules/users/user.routes.js\");\n\nvar _user2 = _interopRequireDefault(_user);\n\nfunction _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }\n\nexports.default = app => {\n  app.use('/api/v1/users', _user2.default);\n};\n\n//# sourceURL=webpack:///./src/modules/index.js?");

/***/ }),

/***/ "./src/modules/users/user.controllers.js":
/*!***********************************************!*\
  !*** ./src/modules/users/user.controllers.js ***!
  \***********************************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";
eval("\n\nObject.defineProperty(exports, \"__esModule\", {\n  value: true\n});\nexports.signup = signup;\nexports.login = login;\n\nvar _bcryptNodejs = __webpack_require__(/*! bcrypt-nodejs */ \"./node_modules/bcrypt-nodejs/bCrypt.js\");\n\nvar _user = __webpack_require__(/*! ./user.model */ \"./src/modules/users/user.model.js\");\n\nvar _user2 = _interopRequireDefault(_user);\n\nfunction _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }\n\nconst client = __webpack_require__(/*! twilio */ \"twilio\")('ACdcf9039846ddd1542aee439dff3170c2', '13519f53eed795553b4901a55c1ebee0');\n\nasync function signup(req, res) {\n  const code = Math.floor(Math.random() * 9000) + 1000;\n  console.log('====================================');\n  console.log(code);\n  console.log('====================================');\n\n  try {\n    // client.messages\n    //   .create({\n    //     body: `this is your code: ${code}`,\n    //     from: '+18509905322',\n    //     to: `+${req.body.phone_number}`,\n    //   });\n    const user = await _user2.default.create(Object.assign({}, req.body, {\n      verify_code: code\n    }));\n    return res.send(user);\n  } catch (e) {\n    return res.status(500).json(e);\n  }\n}\n\nasync function login(req, res) {\n  try {\n    const {\n      phone_number,\n      verify_code\n    } = req.body;\n    const user = await _user2.default.findOne({\n      phone_number\n    });\n\n    if (!user) {\n      return console.log('user not found');\n    }\n\n    if (user.authenticateUser(verify_code)) {\n      res.status(200).send('You are successfully loged in');\n    } else {\n      res.status(400).send('wrong password');\n    }\n  } catch (error) {\n    console.log(error);\n  }\n}\n\n//# sourceURL=webpack:///./src/modules/users/user.controllers.js?");

/***/ }),

/***/ "./src/modules/users/user.model.js":
/*!*****************************************!*\
  !*** ./src/modules/users/user.model.js ***!
  \*****************************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";
eval("\n\nObject.defineProperty(exports, \"__esModule\", {\n  value: true\n});\n\nvar _mongoose = __webpack_require__(/*! mongoose */ \"mongoose\");\n\nvar _mongoose2 = _interopRequireDefault(_mongoose);\n\nvar _bcryptNodejs = __webpack_require__(/*! bcrypt-nodejs */ \"./node_modules/bcrypt-nodejs/bCrypt.js\");\n\nfunction _interopRequireDefault(obj) { return obj && obj.__esModule ? obj : { default: obj }; }\n\nconst UserSchema = new _mongoose.Schema({\n  phone_number: {\n    type: String,\n    trim: true,\n    unique: true,\n    required: [true, 'Phone number is required!']\n  },\n  verify_code: {\n    type: String\n  },\n  phone_status: {\n    type: String\n  }\n});\nUserSchema.pre('save', function (next) {\n  if (this.isModified('verify_code')) {\n    this.verify_code = this._hashCode(this.verify_code);\n  }\n\n  return next();\n});\nUserSchema.methods = {\n  _hashCode(code) {\n    return (0, _bcryptNodejs.hashSync)(code);\n  },\n\n  authenticateUser(code) {\n    return (0, _bcryptNodejs.compareSync)(code, this.verify_code);\n  }\n\n};\nexports.default = _mongoose2.default.model('User', UserSchema);\n\n//# sourceURL=webpack:///./src/modules/users/user.model.js?");

/***/ }),

/***/ "./src/modules/users/user.routes.js":
/*!******************************************!*\
  !*** ./src/modules/users/user.routes.js ***!
  \******************************************/
/*! no static exports found */
/***/ (function(module, exports, __webpack_require__) {

"use strict";
eval("\n\nObject.defineProperty(exports, \"__esModule\", {\n  value: true\n});\n\nvar _express = __webpack_require__(/*! express */ \"express\");\n\nvar _user = __webpack_require__(/*! ./user.controllers */ \"./src/modules/users/user.controllers.js\");\n\nvar userController = _interopRequireWildcard(_user);\n\nfunction _interopRequireWildcard(obj) { if (obj && obj.__esModule) { return obj; } else { var newObj = {}; if (obj != null) { for (var key in obj) { if (Object.prototype.hasOwnProperty.call(obj, key)) newObj[key] = obj[key]; } } newObj.default = obj; return newObj; } }\n\nconst routes = new _express.Router();\nroutes.post('/signup', userController.signup);\nroutes.post('/login', userController.login);\nexports.default = routes;\n\n//# sourceURL=webpack:///./src/modules/users/user.routes.js?");

/***/ }),

/***/ "body-parser":
/*!******************************!*\
  !*** external "body-parser" ***!
  \******************************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("module.exports = require(\"body-parser\");\n\n//# sourceURL=webpack:///external_%22body-parser%22?");

/***/ }),

/***/ "compression":
/*!******************************!*\
  !*** external "compression" ***!
  \******************************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("module.exports = require(\"compression\");\n\n//# sourceURL=webpack:///external_%22compression%22?");

/***/ }),

/***/ "crypto":
/*!*************************!*\
  !*** external "crypto" ***!
  \*************************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("module.exports = require(\"crypto\");\n\n//# sourceURL=webpack:///external_%22crypto%22?");

/***/ }),

/***/ "express":
/*!**************************!*\
  !*** external "express" ***!
  \**************************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("module.exports = require(\"express\");\n\n//# sourceURL=webpack:///external_%22express%22?");

/***/ }),

/***/ "helmet":
/*!*************************!*\
  !*** external "helmet" ***!
  \*************************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("module.exports = require(\"helmet\");\n\n//# sourceURL=webpack:///external_%22helmet%22?");

/***/ }),

/***/ "mongoose":
/*!***************************!*\
  !*** external "mongoose" ***!
  \***************************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("module.exports = require(\"mongoose\");\n\n//# sourceURL=webpack:///external_%22mongoose%22?");

/***/ }),

/***/ "morgan":
/*!*************************!*\
  !*** external "morgan" ***!
  \*************************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("module.exports = require(\"morgan\");\n\n//# sourceURL=webpack:///external_%22morgan%22?");

/***/ }),

/***/ "twilio":
/*!*************************!*\
  !*** external "twilio" ***!
  \*************************/
/*! no static exports found */
/***/ (function(module, exports) {

eval("module.exports = require(\"twilio\");\n\n//# sourceURL=webpack:///external_%22twilio%22?");

/***/ })

/******/ });